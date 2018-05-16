use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use syntex_pos::{Span, DUMMY_SP};
use syntex_syntax::fold::{noop_fold_ty, Folder};
use syntex_syntax::parse::{PResult, ParseSess};
use syntex_syntax::print::pprust;
use syntex_syntax::ptr::P;
use syntex_syntax::symbol::Symbol;
use syntex_syntax::visit::{walk_lifetime, walk_ty, Visitor};
use syntex_syntax::{ast, parse};

use SelfTypeVariant;
use errors::fatal_error;
use types_conv_map::make_unique_rust_typename_if_need;

pub(crate) fn normalized_ty_string(ty: &ast::Ty) -> String {
    struct StripLifetime;
    impl Folder for StripLifetime {
        fn fold_lifetimes(&mut self, _: Vec<ast::Lifetime>) -> Vec<ast::Lifetime> {
            vec![]
        }
        fn fold_opt_lifetime(&mut self, _: Option<ast::Lifetime>) -> Option<ast::Lifetime> {
            None
        }
    }
    let mut strip_lifetime = StripLifetime;
    let ty = noop_fold_ty(P(ty.clone()), &mut strip_lifetime);
    pprust::ty_to_string(&ty)
}

#[derive(Debug, Clone)]
pub(crate) struct RustType {
    pub ty: ast::Ty,
    pub normalized_name: Symbol,
    pub implements: HashSet<Symbol>,
}

impl From<ast::Ty> for RustType {
    fn from(ty: ast::Ty) -> RustType {
        let normalized_name = Symbol::intern(&normalized_ty_string(&ty));
        RustType::new(ty, normalized_name)
    }
}

impl RustType {
    pub(crate) fn new(ty: ast::Ty, norm_name: Symbol) -> RustType {
        RustType {
            ty: ty,
            normalized_name: norm_name,
            implements: HashSet::new(),
        }
    }
    pub(crate) fn implements(mut self, trait_name: &str) -> RustType {
        self.implements.insert(Symbol::intern(trait_name));
        self
    }
}

#[derive(Debug)]
pub(crate) struct GenericTypeConv {
    pub from_ty: ast::Ty,
    pub to_ty: ast::Ty,
    pub code_template: Symbol,
    pub dependency: Rc<RefCell<Option<ast::Item>>>,
    pub generic_params: ast::Generics,
    pub to_foreigner_hint: Option<Symbol>,
}

type TyParamsSubstMap = HashMap<Symbol, Option<P<ast::Ty>>>;

impl GenericTypeConv {
    pub(crate) fn is_conv_possible<'a, OtherRustTypes>(
        &self,
        ty: &RustType,
        goal_ty: Option<&RustType>,
        others: OtherRustTypes,
    ) -> Option<RustType>
    where
        OtherRustTypes: Fn(Symbol) -> Option<&'a RustType>,
    {
        let mut subst_map = TyParamsSubstMap::new();
        trace!(
            "is_conv_possible: begin generic: {:?} => from_ty: {:?} => ty: {}",
            self.generic_params,
            self.from_ty,
            ty.normalized_name
        );
        for ty_p in &self.generic_params.ty_params {
            subst_map.insert(ty_p.ident.name, None);
        }
        if !is_second_subst_of_first(&self.from_ty, &ty.ty, &mut subst_map) {
            return None;
        }
        trace!(
            "is_conv_possible: {:?} is subst of {:?}, check trait bounds",
            ty.ty,
            self.from_ty
        );
        let trait_bounds: HashMap<Symbol, HashSet<Symbol>> = get_trait_bounds(&self.generic_params)
            .into_iter()
            .map(|v| (v.ty_param, v.trait_names))
            .collect();
        let mut has_unbinded = false;
        for (key, val) in &subst_map {
            if let Some(ref val) = *val {
                trace!(
                    "is_conv_possible: key={:?} val={:?}, trait_bounds {:?}",
                    key,
                    val,
                    trait_bounds
                );
                if trait_bounds.get(key).map_or(false, |requires| {
                    let val_name = Symbol::intern(&normalized_ty_string(val));
                    others(val_name).map_or(true, |rt| !requires.is_subset(&rt.implements))
                }) {
                    trace!("is_conv_possible: trait bounds check failed");
                    return None;
                }
            } else {
                has_unbinded = true;
            }
        }
        if has_unbinded {
            trace!("is_conv_possible: has_unbinded: goal_ty {:?}", goal_ty);
            if let Some(goal_ty) = goal_ty {
                is_second_subst_of_first(&self.to_ty, &goal_ty.ty, &mut subst_map);
            }
        }
        let to_ty = replace_all_types_with(&self.to_ty, &subst_map);
        let suffix = if let Some(to_foreigner_hint) = self.to_foreigner_hint {
            assert_eq!(subst_map.len(), 1);
            if let Some(&(key, &Some(ref val))) = subst_map.iter().nth(0).as_ref() {
                let val_name = normalized_ty_string(val);
                let foreign_name = (*to_foreigner_hint.as_str()).replace(&*key.as_str(), &val_name);
                Some(Symbol::intern(&foreign_name))
            } else {
                None
            }
        } else {
            None
        };
        let normalized_name = make_unique_rust_typename_if_need(
            Symbol::intern(&normalized_ty_string(&to_ty)),
            suffix,
        );
        Some(RustType::new(to_ty, normalized_name))
    }
}

/// for example true for Result<T, E> Result<u8, u8>
fn is_second_subst_of_first(
    ty1: &ast::Ty,
    ty2: &ast::Ty,
    subst_map: &mut TyParamsSubstMap,
) -> bool {
    trace!(
        "is_second_substitude_of_first {:?} vs {:?}",
        ty1.node,
        ty2.node
    );
    match (&ty1.node, &ty2.node) {
        (&ast::TyKind::Path(_, ref p1), &ast::TyKind::Path(_, ref p2)) => {
            if p1.segments.len() != p2.segments.len() {
                trace!("is_second_substitude_of_first: path length not match");
                return false;
            }
            if p1.segments.len() == 1 {
                if let Some(subst) = subst_map.get_mut(&p1.segments[0].identifier.name) {
                    if subst.is_none() {
                        *subst = Some(P(ty2.clone()));
                        return true;
                    }
                }
            }
            for (s1, s2) in p1.segments.iter().zip(p2.segments.iter()) {
                if s1.identifier.name != s2.identifier.name {
                    trace!(
                        "is_second_substitude_of_first: id different {} vs {}",
                        s1.identifier.name,
                        s2.identifier.name
                    );
                    return false;
                }
                if !is_second_subst_of_first_ppath(&s1.parameters, &s2.parameters, subst_map) {
                    return false;
                }
            }
            true
        }
        (&ast::TyKind::Rptr(_, ref mut_ty1), &ast::TyKind::Rptr(_, ref mut_ty2)) => {
            if mut_ty1.mutbl != mut_ty2.mutbl {
                trace!("is_second_substitude_of_first mutable not match");
                false
            } else {
                is_second_subst_of_first(&*mut_ty1.ty, &*mut_ty2.ty, subst_map)
            }
        }
        (&ast::TyKind::Slice(ref ty1), &ast::TyKind::Slice(ref ty2)) => {
            is_second_subst_of_first(ty1, ty2, subst_map)
        }
        _ => {
            //TODO: more smart way to strip spans in case of pointers
            let ret = format!("{:?}", ty1.node) == format!("{:?}", ty2.node);
            trace!(
                "is_second_substitude_of_first just check equal {:?} vs {:?} => {}",
                ty1.node,
                ty2.node,
                ret
            );
            ret
        }
    }
}

fn is_second_subst_of_first_ppath(
    p1: &Option<P<ast::PathParameters>>,
    p2: &Option<P<ast::PathParameters>>,
    subst_map: &mut TyParamsSubstMap,
) -> bool {
    match (p1, p2) {
        (&None, &None) => true,
        (&Some(ref p1), &Some(ref p2)) => {
            let p1: &ast::PathParameters = &*p1;
            let p2: &ast::PathParameters = &*p2;
            match (p1, p2) {
                (
                    &ast::PathParameters::AngleBracketed(ref p1),
                    &ast::PathParameters::AngleBracketed(ref p2),
                ) => {
                    if p1.types.len() != p2.types.len() {
                        trace!(
                            "is_second_subst_of_first_ppath: param types len not match {} vs {}",
                            p1.types.len(),
                            p2.types.len()
                        );
                        return false;
                    }
                    for (type_p1, type_p2) in p1.types.iter().zip(p2.types.iter()) {
                        let type_p1_name = Symbol::intern(&normalized_ty_string(type_p1));
                        let real_type_p1: ast::Ty =
                            if let Some(subst) = subst_map.get_mut(&type_p1_name) {
                                match *subst {
                                    Some(ref x) => (**x).clone(),
                                    None => {
                                        *subst = Some(type_p2.clone());
                                        (**type_p2).clone()
                                        //return true;
                                    }
                                }
                            } else {
                                (**type_p1).clone()
                            };
                        trace!("is_second_subst_of_first_ppath: go deeper");
                        if !is_second_subst_of_first(&real_type_p1, type_p2, subst_map) {
                            return false;
                        }
                    }
                    true
                }
                _ => if p1 != p2 {
                    trace!("second_subst_of_first_ppath: p1 != p2 => {:?} {:?}", p1, p2);
                    false
                } else {
                    true
                },
            }
        }
        _ => {
            trace!(
                "second_subst_of_first_ppath:  None/Some check failed for {:?} {:?}",
                p1,
                p2
            );
            false
        }
    }
}

fn replace_all_types_with(in_ty: &ast::Ty, subst_map: &TyParamsSubstMap) -> ast::Ty {
    struct ReplaceTypes<'a> {
        subst_map: &'a TyParamsSubstMap,
    }
    impl<'a> Folder for ReplaceTypes<'a> {
        fn fold_ty(&mut self, t: P<ast::Ty>) -> P<ast::Ty> {
            trace!("ReplaceTypes::fold_ty t {:?}", t);
            let ty_name = Symbol::intern(&normalized_ty_string(&t));
            if let Some(&Some(ref subst)) = self.subst_map.get(&ty_name) {
                subst.clone()
            } else {
                noop_fold_ty(t, self)
            }
        }
        fn fold_angle_bracketed_parameter_data(
            &mut self,
            mut p: ast::AngleBracketedParameterData,
        ) -> ast::AngleBracketedParameterData {
            trace!("ReplaceTypes::fold_angle_bracketed_parameter_data {:?}", p);
            for i in 0..p.types.len() {
                p.types[i] = self.fold_ty(p.types[i].clone());
            }
            p
        }
    }
    trace!(
        "replace_all_types_with in_ty {:?}, subst_map {:?}",
        in_ty,
        subst_map
    );
    let mut rt = ReplaceTypes { subst_map };
    rt.fold_ty(P(in_ty.clone())).unwrap()
}

pub(crate) fn parse_ty(sess: &ParseSess, sp: Span, type_str: Symbol) -> PResult<ast::Ty> {
    let mut parser = parse::new_parser_from_source_str(
        sess,
        format!("{:?}_{:?}", sp.lo, sp.hi),
        type_str.as_str().to_string(),
    );
    match parser.parse_ty() {
        Ok(pty) => {
            let mut ty = pty.unwrap();
            ty.span = sp;
            Ok(ty)
        }
        Err(err) => Err(fatal_error(sess, sp, &err.message())),
    }
}

fn generic_params_new(names: &[&str]) -> ast::Generics {
    let mut ty_params = vec![];
    for name in names {
        ty_params.push(ast::TyParam {
            attrs: ast::ThinVec::new(),
            ident: ast::Ident::from_str(name),
            id: ast::DUMMY_NODE_ID,
            bounds: vec![],
            default: None,
            span: DUMMY_SP,
        });
    }
    ast::Generics {
        lifetimes: vec![],
        ty_params,
        where_clause: ast::WhereClause {
            id: ast::DUMMY_NODE_ID,
            predicates: vec![],
        },
        span: DUMMY_SP,
    }
}

pub(crate) fn if_type_slice_return_elem_type(ty: &ast::Ty) -> Option<ast::Ty> {
    if let ast::TyKind::Rptr(
        _,
        ast::MutTy {
            ref ty,
            mutbl: ast::Mutability::Immutable,
        },
    ) = ty.node
    {
        if let ast::TyKind::Slice(ref ty) = ty.node {
            let ty: &ast::Ty = &*ty;
            Some(ty.clone())
        } else {
            None
        }
    } else {
        None
    }
}

pub(crate) fn if_vec_return_elem_type(ty: &ast::Ty) -> Option<ast::Ty> {
    let sess = ParseSess::new();
    let from_ty = unwrap_presult!(parse_ty(&sess, DUMMY_SP, Symbol::intern("Vec<T>")));
    let to_ty = unwrap_presult!(parse_ty(&sess, DUMMY_SP, Symbol::intern("T")));

    GenericTypeConv {
        from_ty,
        to_ty,
        code_template: Symbol::intern(""),
        dependency: Rc::new(RefCell::new(None)),
        generic_params: generic_params_new(&["T"]),
        to_foreigner_hint: None,
    }.is_conv_possible(&ty.clone().into(), None, |_| None)
        .map(|x| x.ty)
}

pub(crate) fn if_result_return_ok_err_types(ty: &ast::Ty) -> Option<(ast::Ty, ast::Ty)> {
    let ok_ty = {
        let sess = ParseSess::new();
        let from_ty = unwrap_presult!(parse_ty(&sess, DUMMY_SP, Symbol::intern("Result<T, E>")));
        let to_ty = unwrap_presult!(parse_ty(&sess, DUMMY_SP, Symbol::intern("T")));

        GenericTypeConv {
            from_ty,
            to_ty,
            code_template: Symbol::intern(""),
            dependency: Rc::new(RefCell::new(None)),
            generic_params: generic_params_new(&["T", "E"]),
            to_foreigner_hint: None,
        }.is_conv_possible(&ty.clone().into(), None, |_| None)
            .map(|x| x.ty)
    }?;

    let err_ty = {
        let sess = ParseSess::new();
        let from_ty = unwrap_presult!(parse_ty(&sess, DUMMY_SP, Symbol::intern("Result<T, E>")));
        let to_ty = unwrap_presult!(parse_ty(&sess, DUMMY_SP, Symbol::intern("E")));

        GenericTypeConv {
            from_ty,
            to_ty,
            code_template: Symbol::intern(""),
            dependency: Rc::new(RefCell::new(None)),
            generic_params: generic_params_new(&["T", "E"]),
            to_foreigner_hint: None,
        }.is_conv_possible(&ty.clone().into(), None, |_| None)
            .map(|x| x.ty)
    }?;
    Some((ok_ty, err_ty))
}

pub(crate) fn check_if_smart_pointer_return_inner_type(
    ty: &ast::Ty,
    smart_ptr_name: &str,
) -> Option<ast::Ty> {
    let generic_params = generic_params_new(&["T"]);
    let sess = ParseSess::new();
    let from_ty = unwrap_presult!(parse_ty(
        &sess,
        DUMMY_SP,
        Symbol::intern(&format!("{}<T>", smart_ptr_name))
    ));
    let to_ty = unwrap_presult!(parse_ty(&sess, DUMMY_SP, Symbol::intern("T")));

    GenericTypeConv {
        from_ty,
        to_ty,
        code_template: Symbol::intern(""),
        dependency: Rc::new(RefCell::new(None)),
        generic_params,
        to_foreigner_hint: None,
    }.is_conv_possible(&ty.clone().into(), None, |_| None)
        .map(|x| x.ty)
}

pub(crate) fn self_variant(ty: &ast::Ty) -> Option<SelfTypeVariant> {
    //TODO: it is possible just inspect Ty struct without string conversation
    match &*normalized_ty_string(ty) {
        "Self" => Some(SelfTypeVariant::Default),
        "mut Self" => Some(SelfTypeVariant::Mut),
        "&Self" => Some(SelfTypeVariant::Rptr),
        "&mut Self" => Some(SelfTypeVariant::RptrMut),
        _ => None,
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct GenericTraitBound {
    pub ty_param: Symbol,
    pub trait_names: HashSet<Symbol>,
}

pub(crate) fn get_trait_bounds(generic: &ast::Generics) -> Vec<GenericTraitBound> {
    let mut ret = Vec::<GenericTraitBound>::new();
    //    trace!("trait_bounds: begin, generic {:?}", generic);
    for ty_p in &generic.ty_params {
        let mut ret_elem = GenericTraitBound {
            ty_param: ty_p.ident.name,
            trait_names: HashSet::new(),
        };

        for bound in &ty_p.bounds {
            if let ast::TyParamBound::TraitTyParamBound(
                ast::PolyTraitRef { ref trait_ref, .. },
                _,
            ) = *bound
            {
                let trait_name = pprust::path_to_string(&trait_ref.path);
                ret_elem.trait_names.insert(Symbol::intern(&trait_name));
            }
        }
        if !ret_elem.trait_names.is_empty() {
            ret.push(ret_elem);
        }
    }
    for p in &generic.where_clause.predicates {
        if let ast::WherePredicate::BoundPredicate(ast::WhereBoundPredicate {
            ref bounded_ty,
            ref bounds,
            ..
        }) = *p
        {
            let mut ret_elem = GenericTraitBound {
                ty_param: Symbol::intern(&normalized_ty_string(bounded_ty)),
                trait_names: HashSet::new(),
            };

            for bound in &*bounds {
                if let ast::TyParamBound::TraitTyParamBound(
                    ast::PolyTraitRef { ref trait_ref, .. },
                    _,
                ) = *bound
                {
                    let trait_name = pprust::path_to_string(&trait_ref.path);
                    ret_elem.trait_names.insert(Symbol::intern(&trait_name));
                }
            }
            if !ret_elem.trait_names.is_empty() {
                ret.push(ret_elem);
            }
        }
    }
    //    trace!("trace_bounds: end {:?}", ret);
    ret
}

pub(crate) fn list_lifetimes(ty: &ast::Ty) -> Vec<Symbol> {
    struct CatchLifetimes(Vec<Symbol>);
    impl<'a> Visitor<'a> for CatchLifetimes {
        fn visit_lifetime(&mut self, lifetime: &'a ast::Lifetime) {
            self.0.push(lifetime.name);
            walk_lifetime(self, lifetime)
        }
    }
    let mut catch_lifetimes = CatchLifetimes(Vec::new());
    walk_ty(&mut catch_lifetimes, ty);
    catch_lifetimes.0
}

pub(crate) fn code_to_item<'a>(
    sess: &'a ParseSess,
    for_func_name: &str,
    code: &str,
) -> PResult<'a, Vec<P<ast::Item>>> {
    let mut parser = parse::new_parser_from_source_str(sess, for_func_name.into(), code.into());

    let krate = parser.parse_crate_mod()?;
    Ok(krate.module.items)
}

pub(crate) fn if_option_return_some_type(ty: &ast::Ty) -> Option<ast::Ty> {
    let generic_params = generic_params_new(&["T"]);
    let sess = ParseSess::new();
    let from_ty = unwrap_presult!(parse_ty(&sess, DUMMY_SP, Symbol::intern("Option<T>")));
    let to_ty = unwrap_presult!(parse_ty(&sess, DUMMY_SP, Symbol::intern("T")));

    GenericTypeConv {
        from_ty,
        to_ty,
        code_template: Symbol::intern(""),
        dependency: Rc::new(RefCell::new(None)),
        generic_params,
        to_foreigner_hint: None,
    }.is_conv_possible(&ty.clone().into(), None, |_| None)
        .map(|x| x.ty)
}

pub(crate) fn get_ref_type(ty: &ast::Ty, mutbl: ast::Mutability) -> ast::Ty {
    ast::Ty {
        id: ast::DUMMY_NODE_ID,
        span: ty.span,
        node: ast::TyKind::Rptr(
            None,
            ast::MutTy {
                mutbl: mutbl,
                ty: P(ty.clone()),
            },
        ),
    }
}

#[cfg(test)]
#[macro_use]
#[path = "test_helper.rs"]
mod test_helper;

#[cfg(test)]
mod tests {
    use self::test_helper::*;
    use super::*;
    use syntex_syntax::parse;
    use syntex_syntax::parse::ParseSess;

    #[test]
    fn test_normalize_ty() {
        let sess = parse::ParseSess::new();

        assert_eq!(&normalized_ty_string(&str_to_ty(&sess, "&str")), "&str");
        assert_eq!(&normalized_ty_string(&str_to_ty(&sess, "&'a str")), "&str");
        assert_eq!(&normalized_ty_string(&str_to_ty(&sess, "string")), "string");
        assert_eq!(&normalized_ty_string(&str_to_ty(&sess, "()")), "()");
        assert_eq!(
            &normalized_ty_string(&str_to_ty(&sess, "Foo<'a, T>")),
            "Foo<T>"
        );
    }

    #[test]
    fn generic_type_conv_find() {
        logger_init();
        let sess = parse::ParseSess::new();
        let generic = get_generic_params_from_item(
            &sess,
            r#"
#[swig_to_foreigner_hint = "T []"]
impl<T: SwigForeignClass> SwigFrom<Vec<T>> for jobjectArray {
    fn swig_from(x: Vec<T>, env: *mut JNIEnv) -> Self {
        vec_of_objects_to_jobject_array(x, <T>::jni_class_name(), env)
    }
}
"#,
        );

        let foo_spec = RustType::new(str_to_ty(&sess, "Foo"), Symbol::intern("Foo"))
            .implements("SwigForeignClass");
        let refcell_foo_spec = RustType::new(
            str_to_ty(&sess, "RefCell<Foo>"),
            Symbol::intern("RefCell<Foo>"),
        ).implements("SwigForeignClass");

        fn check_subst<'a, FT: Fn(Symbol) -> Option<&'a RustType>>(
            sess: &ParseSess,
            generic: &ast::Generics,
            from_ty_name: &str,
            to_ty_name: &str,
            ty_check_name: &str,
            expect_to_ty_name: &str,
            map_others: FT,
        ) -> RustType {
            trace!(
                "check_subst: conv {} -> {} with {}",
                from_ty_name,
                to_ty_name,
                ty_check_name
            );
            let ret_ty: RustType =
                GenericTypeConv {
                    from_ty: str_to_ty(sess, from_ty_name),
                    to_ty: str_to_ty(sess, to_ty_name),
                    code_template: Symbol::intern(""),
                    dependency: Rc::new(RefCell::new(None)),
                    generic_params: generic.clone(),
                    to_foreigner_hint: None,
                }.is_conv_possible(&str_to_ty(&sess, ty_check_name).into(), None, map_others)
                    .expect("check subst failed");
            assert_eq!(&*ret_ty.normalized_name.as_str(), expect_to_ty_name);

            ret_ty
        }

        check_subst(
            &sess,
            &generic,
            "Rc<T>",
            "jlong",
            "Rc<RefCell<Foo>>",
            "jlong",
            |name| {
                trace!("test rt map, check name {:?}", name);
                if name == Symbol::intern("Foo") {
                    Some(&foo_spec)
                } else if name == Symbol::intern("RefCell<Foo>") {
                    Some(&refcell_foo_spec)
                } else {
                    None
                }
            },
        );

        check_subst(
            &sess,
            &generic,
            "Vec<T>",
            "jobjectArray",
            "Vec<Foo>",
            "jobjectArray",
            |name| {
                if name == Symbol::intern("Foo") {
                    Some(&foo_spec)
                } else {
                    None
                }
            },
        );

        let generic = get_generic_params_from_item(
            &sess,
            r#"
impl<'a, T> SwigFrom<&'a RefCell<T>> for RefMut<'a, T> {
    fn swig_from(m: &'a RefCell<T>, _: *mut JNIEnv) -> RefMut<'a, T> {
        m.borrow_mut()
    }
}
"#,
        );

        check_subst(
            &sess,
            &generic,
            "&RefCell<T>",
            "RefMut<T>",
            "&RefCell<Foo>",
            "RefMut<Foo>",
            |_| None,
        );

        check_subst(
            &sess,
            &generic,
            "&Rc<T>",
            "&T",
            "&Rc<RefCell<Foo>>",
            "&RefCell<Foo>",
            |_| None,
        );

        check_subst(
            &sess,
            &generic,
            "Arc<Mutex<T>>",
            "&Mutex<T>",
            "Arc<Mutex<Foo>>",
            "&Mutex<Foo>",
            |_| None,
        );

        let mutex_guard_foo = check_subst(
            &sess,
            &generic,
            "&Mutex<T>",
            "MutexGuard<T>",
            "&Mutex<Foo>",
            "MutexGuard<Foo>",
            |_| None,
        );
        assert_eq!(
            &*GenericTypeConv {
                from_ty: str_to_ty(&sess, "MutexGuard<T>"),
                to_ty: str_to_ty(&sess, "&T"),
                code_template: Symbol::intern(""),
                dependency: Rc::new(RefCell::new(None)),
                generic_params: generic.clone(),
                to_foreigner_hint: None,
            }.is_conv_possible(&mutex_guard_foo, None, |name| {
                if name == Symbol::intern("Foo") {
                    Some(&foo_spec)
                } else {
                    None
                }
            })
                .unwrap()
                .normalized_name
                .as_str(),
            "&Foo"
        );

        let box_foo: RustType = str_to_ty(&sess, "Box<Foo>").into();

        assert_eq!(
            &*GenericTypeConv {
                from_ty: str_to_ty(&sess, "jlong"),
                to_ty: str_to_ty(&sess, "Box<T>"),
                code_template: Symbol::intern(""),
                dependency: Rc::new(RefCell::new(None)),
                generic_params: generic,
                to_foreigner_hint: None,
            }.is_conv_possible(&str_to_ty(&sess, "jlong").into(), Some(&box_foo), |_| None)
                .unwrap()
                .normalized_name
                .as_str(),
            "Box<Foo>"
        );

        let generic = get_generic_params_from_item(
            &sess,
            r#"
impl<T: SwigForeignClass> SwigFrom<Box<T>> for jlong {
    fn swig_from(x: Box<T>, _: *mut JNIEnv) -> jlong {
        unimplemented!();
    }
}
"#,
        );
        check_subst(&sess, &generic, "T", "Box<T>", "Foo", "Box<Foo>", |name| {
            if name == Symbol::intern("Foo") {
                Some(&foo_spec)
            } else {
                None
            }
        });

        let generic = get_generic_params_from_item(
            &sess,
            r#"
impl<T, E> SwigFrom<Result<T,E>> for T {
    fn swig_from(v: Result<T, E>, _: *mut JNIEnv) -> T {
        unimplemented!();
    }
}
"#,
        );
        check_subst(
            &sess,
            &generic,
            "Result<T, E>",
            "T",
            "Result<u8, &'static str>",
            "u8",
            |_| None,
        );
    }

    #[test]
    fn test_work_with_result() {
        logger_init();
        let sess = ParseSess::new();
        assert_eq!(
            if_result_return_ok_err_types(&str_to_ty(&sess, "Result<bool, String>"))
                .map(|(x, y)| (normalized_ty_string(&x), normalized_ty_string(&y)))
                .unwrap(),
            ("bool".to_string(), "String".to_string())
        );
    }

    #[test]
    fn test_work_with_vec() {
        logger_init();
        let sess = ParseSess::new();
        assert_eq!(
            if_vec_return_elem_type(&str_to_ty(&sess, "Vec<bool>"))
                .map(|x| normalized_ty_string(&x))
                .unwrap(),
            ("bool".to_string())
        );
    }

    #[test]
    fn test_work_with_rc() {
        logger_init();
        let sess = ParseSess::new();
        let ty =
            check_if_smart_pointer_return_inner_type(&str_to_ty(&sess, "Rc<RefCell<bool>>"), "Rc")
                .unwrap();
        assert_eq!(normalized_ty_string(&ty), "RefCell<bool>".to_string());

        let generic_params = generic_params_new(&["T"]);
        assert_eq!(
            &*GenericTypeConv {
                from_ty: str_to_ty(&sess, "RefCell<T>"),
                to_ty: str_to_ty(&sess, "T"),
                code_template: Symbol::intern(""),
                dependency: Rc::new(RefCell::new(None)),
                generic_params,
                to_foreigner_hint: None,
            }.is_conv_possible(&ty.into(), None, |_| None)
                .unwrap()
                .normalized_name
                .as_str(),
            "bool"
        );
    }

    #[test]
    fn test_trait_bounds() {
        logger_init();
        let sess = ParseSess::new();
        assert_eq!(
            get_trait_bounds(&get_generic_params_from_item(
                &sess,
                "impl<T> Foo for Boo {}"
            )),
            vec![]
        );
        assert_eq!(
            get_trait_bounds(&get_generic_params_from_item(
                &sess,
                "impl<T: Moo> Foo for Boo {}"
            )),
            vec![
                GenericTraitBound {
                    ty_param: Symbol::intern("T"),
                    trait_names: vec![Symbol::intern("Moo")].into_iter().collect(),
                },
            ]
        );

        assert_eq!(
            get_trait_bounds(&get_generic_params_from_item(
                &sess,
                "impl<T> Foo for Boo where T: Moo {}"
            )),
            vec![
                GenericTraitBound {
                    ty_param: Symbol::intern("T"),
                    trait_names: vec![Symbol::intern("Moo")].into_iter().collect(),
                },
            ]
        );
    }

    #[test]
    fn test_list_lifetimes() {
        let sess = ParseSess::new();
        let my_list_lifetimes = |code| -> Vec<String> {
            let ret = list_lifetimes(&str_to_ty(&sess, code));
            ret.iter().map(|v| v.as_str().to_string()).collect()
        };
        assert_eq!(vec!["'a"], my_list_lifetimes("Rc<RefCell<Foo<'a>>>"));
    }

    fn str_to_ty(sess: &ParseSess, code: &str) -> ast::Ty {
        let mut parser =
            parse::new_parser_from_source_str(&sess, "tests::str_to_ty".into(), code.to_string());
        parser.parse_ty().unwrap().unwrap()
    }

    fn get_generic_params_from_item(sess: &ParseSess, item_code: &str) -> ast::Generics {
        let mut parser =
            parse::new_parser_from_source_str(&sess, "generic".into(), item_code.into());
        let item = unwrap_presult!(parser.parse_item()).unwrap();
        match *item {
            ast::Item {
                node:
                    ast::ItemKind::Impl(
                        ast::Unsafety::Normal,
                        ast::ImplPolarity::Positive,
                        ref generic,
                        _,
                        _,
                        _,
                    ),
                ..
            } => generic.clone(),
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_work_with_option() {
        let sess = ParseSess::new();
        assert_eq!(
            normalized_ty_string(
                &if_option_return_some_type(&str_to_ty(&sess, "Option<String>")).unwrap()
            ),
            "String".to_string()
        );
    }

}
