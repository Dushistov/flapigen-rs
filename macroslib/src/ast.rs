mod collections;

use std::{
    cell::RefCell,
    fmt::Display,
    hash::{Hash, Hasher},
    mem,
    rc::Rc,
};

use log::{log_enabled, trace};
use proc_macro2::{Ident, Span, TokenStream};
use quote::{quote, ToTokens};
use rustc_hash::FxHashMap;
use smallvec::SmallVec;
use smol_str::SmolStr;
use syn::{
    parse_quote,
    visit::{visit_lifetime, Visit},
    visit_mut::{
        visit_angle_bracketed_generic_arguments_mut, visit_type_mut, visit_type_reference_mut,
        VisitMut,
    },
    Type,
};

use self::collections::{ImplementsSet, TraitNamesSet, TyParamsSubstItem, TyParamsSubstMap};
use crate::typemap::{make_unique_rust_typename, make_unique_rust_typename_if_need};

#[derive(Debug)]
pub(crate) struct TypeName {
    pub typename: SmolStr,
    pub span: Span,
}

impl PartialEq for TypeName {
    fn eq(&self, o: &Self) -> bool {
        self.typename == o.typename
    }
}

impl Eq for TypeName {}

impl Hash for TypeName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.typename.hash(state)
    }
}

impl TypeName {
    pub fn new(typename: SmolStr, span: Span) -> Self {
        TypeName { typename, span }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct RustType {
    pub ty: syn::Type,
    pub normalized_name: SmolStr,
    pub implements: ImplementsSet,
}

impl Display for RustType {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::result::Result<(), core::fmt::Error> {
        write!(f, "{}", self.normalized_name)
    }
}

impl From<syn::Type> for RustType {
    fn from(ty: syn::Type) -> RustType {
        let normalized_name = normalize_ty_lifetimes(&ty);
        RustType::new(ty, normalized_name)
    }
}

impl RustType {
    pub(crate) fn new<S>(ty: syn::Type, norm_name: S) -> RustType
    where
        S: Into<SmolStr>,
    {
        RustType {
            ty,
            normalized_name: norm_name.into(),
            implements: ImplementsSet::default(),
        }
    }
    pub(crate) fn implements(mut self, trait_name: &str) -> RustType {
        self.implements.insert(trait_name.into());
        self
    }
    pub(crate) fn merge(&mut self, other: &RustType) {
        self.ty = other.ty.clone();
        self.normalized_name = other.normalized_name.clone();
        self.implements.insert_set(&other.implements);
    }
}

struct NormalizeTyLifetimesCache {
    inner: FxHashMap<syn::Type, Box<str>>,
}

impl NormalizeTyLifetimesCache {
    fn new() -> Self {
        NormalizeTyLifetimesCache {
            inner: FxHashMap::default(),
        }
    }
    fn insert(&mut self, ty: &syn::Type, val: String) -> &'static str {
        self.inner.insert(ty.clone(), val.into_boxed_str());
        self.get(ty).expect("empty after insert")
    }
    fn get(&self, ty: &syn::Type) -> Option<&'static str> {
        self.inner.get(ty).map(|x| unsafe { mem::transmute(&**x) })
    }
}

fn with_normalize_ty_lifetimes_cache<T, F: FnOnce(&mut NormalizeTyLifetimesCache) -> T>(f: F) -> T {
    thread_local!(static INTERNER: RefCell<NormalizeTyLifetimesCache> = {
        RefCell::new(NormalizeTyLifetimesCache::new())
    });
    INTERNER.with(|interner| f(&mut *interner.borrow_mut()))
}

pub(crate) fn normalize_ty_lifetimes(ty: &syn::Type) -> &'static str {
    if let Some(cached_str) = with_normalize_ty_lifetimes_cache(|cache| cache.get(ty)) {
        return cached_str;
    }

    struct StripLifetime;
    impl VisitMut for StripLifetime {
        fn visit_type_reference_mut(&mut self, i: &mut syn::TypeReference) {
            i.lifetime = None;
            visit_type_reference_mut(self, i)
        }
        fn visit_angle_bracketed_generic_arguments_mut(
            &mut self,
            i: &mut syn::AngleBracketedGenericArguments,
        ) {
            let mut args =
                syn::punctuated::Punctuated::<syn::GenericArgument, syn::token::Comma>::new();
            mem::swap(&mut args, &mut i.args);
            i.args = args
                .into_iter()
                .filter(|x| {
                    if let syn::GenericArgument::Lifetime(_) = x {
                        false
                    } else {
                        true
                    }
                })
                .collect();
            visit_angle_bracketed_generic_arguments_mut(self, i);
        }
    }

    let mut strip_lifetime = StripLifetime;
    let mut new_ty = ty.clone();
    strip_lifetime.visit_type_mut(&mut new_ty);
    let type_str = new_ty.into_token_stream().to_string();

    with_normalize_ty_lifetimes_cache(|cache| cache.insert(ty, type_str))
}

#[derive(Debug)]
pub(crate) struct GenericTypeConv {
    pub from_ty: syn::Type,
    pub to_ty: syn::Type,
    pub code_template: String,
    pub dependency: Rc<RefCell<Option<TokenStream>>>,
    pub generic_params: syn::Generics,
    pub to_foreigner_hint: Option<String>,
    pub from_foreigner_hint: Option<String>,
}

impl GenericTypeConv {
    pub(crate) fn simple_new(
        from_ty: Type,
        to_ty: Type,
        generic_params: syn::Generics,
    ) -> GenericTypeConv {
        GenericTypeConv {
            from_ty,
            to_ty,
            code_template: String::new(),
            dependency: Rc::new(RefCell::new(None)),
            generic_params,
            to_foreigner_hint: None,
            from_foreigner_hint: None,
        }
    }

    pub(crate) fn is_conv_possible<'a, OtherRustTypes>(
        &self,
        ty: &RustType,
        goal_ty: Option<&RustType>,
        others: OtherRustTypes,
    ) -> Option<RustType>
    where
        OtherRustTypes: Fn(&str) -> Option<&'a RustType>,
    {
        let mut subst_map = TyParamsSubstMap::default();
        trace!(
            "is_conv_possible: begin generic: {:?} => from_ty: {:?} => ty: {}",
            self.generic_params,
            self.from_ty,
            ty.normalized_name
        );
        for ty_p in self.generic_params.type_params() {
            subst_map.insert(&ty_p.ident, None);
        }
        if !is_second_subst_of_first(&self.from_ty, &ty.ty, &mut subst_map) {
            return None;
        }
        trace!(
            "is_conv_possible: {:?} is subst of {:?}, check trait bounds",
            ty.ty,
            self.from_ty
        );
        let trait_bounds = get_trait_bounds(&self.generic_params);
        let mut has_unbinded = false;
        for subst_it in subst_map.as_slice() {
            if let Some(ref val) = subst_it.ty {
                trace!(
                    "is_conv_possible: subst_it={:?}, trait_bounds {:?}",
                    *subst_it,
                    trait_bounds
                );
                if trait_bounds
                    .iter()
                    .position(|it| it.ty_param.as_ref() == subst_it.ident)
                    .map_or(false, |idx| {
                        let requires = &trait_bounds[idx].trait_names;
                        let val_name = normalize_ty_lifetimes(val);

                        others(val_name).map_or(true, |rt| !rt.implements.contains_subset(requires))
                    })
                {
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

        /*
        For example if from type jobjectArray, and we use rule
        from jobjectArray -> Vec<T> where T: ForeignClass,
        then we filter jobjectArray^java.lang.String types as input for our rule
        */
        if let Some(ref from_foreigner_hint) = self.from_foreigner_hint {
            trace!("suffix is_conv_possible has from_foreigner_hint");
            assert_eq!(subst_map.len(), 1);
            if let Some(TyParamsSubstItem {
                ident: key,
                ty: Some(ref val),
            }) = subst_map.as_slice().iter().nth(0).as_ref()
            {
                let val_name = normalize_ty_lifetimes(val);
                let foreign_name =
                    (*from_foreigner_hint.as_str()).replace(&key.to_string(), &val_name);
                let clean_from_ty = normalize_ty_lifetimes(&self.from_ty);
                if ty.normalized_name != make_unique_rust_typename(&clean_from_ty, &foreign_name) {
                    trace!("is_conv_possible: check failed by from_foreigner_hint check");
                    return None;
                }
            }
        }

        let to_ty = replace_all_types_with(&self.to_ty, &subst_map);
        let to_suffix = if let Some(ref to_foreigner_hint) = self.to_foreigner_hint {
            assert_eq!(subst_map.len(), 1);
            if let Some(TyParamsSubstItem {
                ident: key,
                ty: Some(ref val),
            }) = subst_map.as_slice().iter().nth(0).as_ref()
            {
                let val_name = normalize_ty_lifetimes(val);
                let foreign_name =
                    (*to_foreigner_hint.as_str()).replace(&key.to_string(), &val_name);
                Some(foreign_name)
            } else {
                None
            }
        } else {
            None
        };
        let normalized_name = make_unique_rust_typename_if_need(
            normalize_ty_lifetimes(&to_ty).to_string(),
            to_suffix,
        );
        Some(RustType::new(to_ty, normalized_name))
    }
}

/// for example true for Result<T, E> Result<u8, u8>
fn is_second_subst_of_first(ty1: &Type, ty2: &Type, subst_map: &mut TyParamsSubstMap) -> bool {
    trace!("is_second_substitude_of_first {:?} vs {:?}", ty1, ty2);
    match (ty1, ty2) {
        (
            Type::Path(syn::TypePath { path: ref p1, .. }),
            Type::Path(syn::TypePath { path: ref p2, .. }),
        ) => {
            if p1.segments.len() != p2.segments.len() {
                trace!("is_second_substitude_of_first: path length not match");
                return false;
            }
            if p1.segments.len() == 1 {
                if let Some(subst) = subst_map.get_mut(&p1.segments[0].ident) {
                    if subst.is_none() {
                        *subst = Some(ty2.clone());
                        return true;
                    }
                }
            }
            for (s1, s2) in p1.segments.iter().zip(p2.segments.iter()) {
                if s1.ident != s2.ident {
                    trace!(
                        "is_second_substitude_of_first: id different {} vs {}",
                        s1.ident,
                        s2.ident
                    );
                    return false;
                }
                if !is_second_subst_of_first_ppath(&s1.arguments, &s2.arguments, subst_map) {
                    return false;
                }
            }
            true
        }
        (Type::Reference(ref mut_ty1), Type::Reference(ref mut_ty2)) => {
            if mut_ty1.mutability != mut_ty2.mutability {
                trace!("is_second_substitude_of_first mutable not match");
                false
            } else {
                is_second_subst_of_first(&*mut_ty1.elem, &*mut_ty2.elem, subst_map)
            }
        }
        (Type::Slice(ref ty1), Type::Slice(ref ty2)) => {
            is_second_subst_of_first(&*ty1.elem, &*ty2.elem, subst_map)
        }
        (Type::Tuple(ref ty1), Type::Tuple(ref ty2)) => {
            if ty1.elems.len() != ty2.elems.len() {
                trace!("is_second_subst_of_first: tuple elems length not match");
                return false;
            }
            for (ty1_e, ty2_e) in ty1.elems.iter().zip(ty2.elems.iter()) {
                if !is_second_subst_of_first(ty1_e, ty2_e, subst_map) {
                    return false;
                }
            }
            true
        }
        _ => {
            let ret = ty1 == ty2;
            trace!(
                "is_second_substitude_of_first just check equal {:?} vs {:?} => {}",
                ty1,
                ty2,
                ret
            );
            ret
        }
    }
}

fn is_second_subst_of_first_ppath(
    p1: &syn::PathArguments,
    p2: &syn::PathArguments,
    subst_map: &mut TyParamsSubstMap,
) -> bool {
    match (p1, p2) {
        (
            syn::PathArguments::AngleBracketed(ref p1),
            syn::PathArguments::AngleBracketed(ref p2),
        ) => {
            if p1.args.len() != p2.args.len() {
                trace!(
                    "is_second_subst_of_first_ppath: param types len not match {} vs {}",
                    p1.args.len(),
                    p2.args.len()
                );
                return false;
            }
            for (type_p1, type_p2) in p1.args.iter().zip(p2.args.iter()) {
                let (type_p1, type_p2) = match (type_p1, type_p2) {
                    (syn::GenericArgument::Type(ref ty1), syn::GenericArgument::Type(ref ty2)) => {
                        (ty1, ty2)
                    }
                    _ => {
                        if type_p1 != type_p2 {
                            trace!(
                                "is_second_subst_of_first_ppath: generic args cmp {:?} != {:?}",
                                type_p1,
                                type_p2
                            );
                            return false;
                        } else {
                            continue;
                        }
                    }
                };
                let type_p1_name = normalize_ty_lifetimes(type_p1);
                let real_type_p1: Type =
                    if let Some(subst) = subst_map.get_mut_by_str(&type_p1_name) {
                        match *subst {
                            Some(ref x) => (*x).clone(),
                            None => {
                                *subst = Some(type_p2.clone());
                                (*type_p2).clone()
                                //return true;
                            }
                        }
                    } else {
                        (*type_p1).clone()
                    };
                trace!("is_second_subst_of_first_ppath: go deeper");
                if !is_second_subst_of_first(&real_type_p1, type_p2, subst_map) {
                    return false;
                }
            }
            true
        }
        _ => {
            if p1 != p2 {
                trace!("second_subst_of_first_ppath: p1 != p2 => {:?} {:?}", p1, p2);
                false
            } else {
                true
            }
        }
    }
}

fn replace_all_types_with(in_ty: &Type, subst_map: &TyParamsSubstMap) -> Type {
    struct ReplaceTypes<'a, 'b> {
        subst_map: &'a TyParamsSubstMap<'b>,
    }
    impl<'a, 'b> VisitMut for ReplaceTypes<'a, 'b> {
        fn visit_type_mut(&mut self, t: &mut Type) {
            let ty_name = normalize_ty_lifetimes(t);
            if let Some(&Some(ref subst)) = self.subst_map.get(&ty_name) {
                *t = subst.clone();
            } else {
                visit_type_mut(self, t);
            }
        }
    }
    if log_enabled!(log::Level::Trace) {
        trace!(
            "replace_all_types_with in_ty {}, subst_map {:?}",
            in_ty.into_token_stream().to_string(),
            subst_map
        );
    }
    let mut rt = ReplaceTypes { subst_map };
    let mut new_ty = in_ty.clone();
    rt.visit_type_mut(&mut new_ty);
    new_ty
}

#[derive(Debug)]
pub(crate) enum TyParamRef<'a> {
    Ref(&'a Ident),
    Own(Ident),
}

impl<'a> PartialEq for TyParamRef<'_> {
    fn eq<'b>(&self, o: &TyParamRef<'b>) -> bool {
        self.as_ref() == o.as_ref()
    }
}

impl<'a> AsRef<Ident> for TyParamRef<'_> {
    fn as_ref(&self) -> &Ident {
        match self {
            TyParamRef::Ref(x) => x,
            TyParamRef::Own(x) => &x,
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct GenericTraitBound<'a> {
    pub(crate) ty_param: TyParamRef<'a>,
    pub(crate) trait_names: TraitNamesSet<'a>,
}

pub(crate) type GenericTraitBoundVec<'a> = SmallVec<[GenericTraitBound<'a>; 10]>;

pub(crate) fn get_trait_bounds(generic: &syn::Generics) -> GenericTraitBoundVec {
    let mut ret = GenericTraitBoundVec::new();

    for ty_p in generic.type_params() {
        if ty_p.bounds.is_empty() {
            continue;
        }
        let mut ret_elem = GenericTraitBound {
            ty_param: TyParamRef::Ref(&ty_p.ident),
            trait_names: TraitNamesSet::default(),
        };

        for bound in &ty_p.bounds {
            if let syn::TypeParamBound::Trait(syn::TraitBound {
                path: ref trait_path,
                ..
            }) = *bound
            {
                ret_elem.trait_names.insert(&trait_path);
            }
        }
        if !ret_elem.trait_names.is_empty() {
            ret.push(ret_elem);
        }
    }
    if let Some(ref where_clause) = generic.where_clause {
        for p in &where_clause.predicates {
            if let syn::WherePredicate::Type(syn::PredicateType {
                ref bounded_ty,
                ref bounds,
                ..
            }) = *p
            {
                let mut ret_elem = GenericTraitBound {
                    ty_param: TyParamRef::Own(Ident::new(
                        &normalize_ty_lifetimes(bounded_ty),
                        Span::call_site(),
                    )),
                    trait_names: TraitNamesSet::default(),
                };

                for bound in bounds {
                    if let syn::TypeParamBound::Trait(syn::TraitBound {
                        path: ref trait_path,
                        ..
                    }) = *bound
                    {
                        ret_elem.trait_names.insert(&trait_path);
                    }
                }
                if !ret_elem.trait_names.is_empty() {
                    ret.push(ret_elem);
                }
            }
        }
    }

    ret
}

pub(crate) fn if_type_slice_return_elem_type(ty: &Type, accept_mutbl_slice: bool) -> Option<&Type> {
    if let syn::Type::Reference(syn::TypeReference {
        ref elem,
        mutability,
        ..
    }) = ty
    {
        if mutability.is_some() && !accept_mutbl_slice {
            return None;
        }
        if let syn::Type::Slice(syn::TypeSlice { ref elem, .. }) = **elem {
            Some(&*elem)
        } else {
            None
        }
    } else {
        None
    }
}

pub(crate) fn if_option_return_some_type(ty: &Type) -> Option<Type> {
    let generic_params: syn::Generics = parse_quote! { <T> };
    let from_ty: Type = parse_quote! { Option<T> };
    let to_ty: Type = parse_quote! { T };

    GenericTypeConv::simple_new(from_ty, to_ty, generic_params)
        .is_conv_possible(&ty.clone().into(), None, |_| None)
        .map(|x| x.ty)
}

pub(crate) fn if_vec_return_elem_type(ty: &Type) -> Option<Type> {
    let from_ty: Type = parse_quote! { Vec<T> };
    let to_ty: Type = parse_quote! { T };
    let generic_params: syn::Generics = parse_quote! { <T> };

    GenericTypeConv::simple_new(from_ty, to_ty, generic_params)
        .is_conv_possible(&ty.clone().into(), None, |_| None)
        .map(|x| x.ty)
}

pub(crate) fn if_result_return_ok_err_types(ty: &Type) -> Option<(Type, Type)> {
    let from_ty: Type = parse_quote! { Result<T, E> };
    let ok_ty: Type = parse_quote! { T };
    let err_ty: Type = parse_quote! { E };
    let generic_params: syn::Generics = parse_quote! { <T, E> };

    let ok_ty = {
        GenericTypeConv::simple_new(from_ty.clone(), ok_ty, generic_params.clone())
            .is_conv_possible(&ty.clone().into(), None, |_| None)
            .map(|x| x.ty)
    }?;

    let err_ty = {
        GenericTypeConv::simple_new(from_ty, err_ty, generic_params)
            .is_conv_possible(&ty.clone().into(), None, |_| None)
            .map(|x| x.ty)
    }?;
    Some((ok_ty, err_ty))
}

pub(crate) fn check_if_smart_pointer_return_inner_type(
    ty: &Type,
    smart_ptr_name: &str,
) -> Option<Type> {
    let generic_params: syn::Generics = parse_quote! { <T> };
    let from_ty: Type =
        syn::parse_str(&format!("{}<T>", smart_ptr_name)).expect("smart pointer parse error");
    let to_ty: Type = parse_quote! { T };

    GenericTypeConv::simple_new(from_ty, to_ty, generic_params)
        .is_conv_possible(&ty.clone().into(), None, |_| None)
        .map(|x| x.ty)
}

pub(crate) fn fn_arg_type(a: &syn::FnArg) -> &syn::Type {
    use syn::FnArg::*;
    match a {
        SelfRef(_) | SelfValue(_) => panic!("internal error: fn_arg_type for self type"),
        Inferred(_) => panic!("internal erorr: fn_arg_type for inferred"),
        Captured(syn::ArgCaptured { ref ty, .. }) | Ignored(ref ty) => ty,
    }
}

pub(crate) fn list_lifetimes(ty: &Type) -> Vec<String> {
    struct CatchLifetimes(Vec<String>);
    impl<'ast> Visit<'ast> for CatchLifetimes {
        fn visit_lifetime(&mut self, lifetime: &syn::Lifetime) {
            self.0.push(format!("'{}", lifetime.ident.to_string()));
            visit_lifetime(self, lifetime)
        }
    }
    let mut catch_lifetimes = CatchLifetimes(Vec::new());
    catch_lifetimes.visit_type(ty);
    catch_lifetimes.0
}

pub(crate) fn change_span(ty: &mut Type, sp: Span) {
    struct ChangeSpan(Option<Span>);
    impl VisitMut for ChangeSpan {
        fn visit_span_mut(&mut self, i: &mut Span) {
            if let Some(sp) = self.0 {
                *i = sp;
            }
            self.0 = None;
        }
    }
    let mut change_span = ChangeSpan(Some(sp));
    change_span.visit_type_mut(ty);
}

pub(crate) struct DisplayToTokens<'a, T: ToTokens>(pub &'a T);

impl<T> Display for DisplayToTokens<'_, T>
where
    T: ToTokens,
{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::result::Result<(), core::fmt::Error> {
        write!(f, "{}", self.0.into_token_stream().to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use smallvec::smallvec;

    #[test]
    fn test_normalize_ty() {
        assert_eq!(normalize_ty_lifetimes(&str_to_ty("&str")), "& str");
        assert_eq!(normalize_ty_lifetimes(&str_to_ty("&'a str")), "& str");
        assert_eq!(normalize_ty_lifetimes(&str_to_ty("string")), "string");
        assert_eq!(normalize_ty_lifetimes(&str_to_ty("()")), "( )");
        assert_eq!(
            normalize_ty_lifetimes(&str_to_ty("Foo<'a, T>")),
            "Foo < T >"
        );
    }

    macro_rules! get_generic_params_from_code {
        ($($tt:tt)*) => {{
            let item: syn::ItemImpl = parse_quote! { $($tt)* };
            item.generics
        }}
    }

    #[test]
    fn generic_type_conv_find() {
        let _ = env_logger::try_init();
        let generic = get_generic_params_from_code! {
            #[swig_to_foreigner_hint = "T []"]
            impl<T: SwigForeignClass> SwigFrom<Vec<T>> for jobjectArray {
                fn swig_from(x: Vec<T>, env: *mut JNIEnv) -> Self {
                    vec_of_objects_to_jobject_array(x, <T>::jni_class_name(), env)
                }
            }
        };

        let foo_spec = RustType::new(str_to_ty("Foo"), "Foo").implements("SwigForeignClass");

        let refcell_foo_spec =
            RustType::new(str_to_ty("RefCell<Foo>"), "RefCell<Foo>").implements("SwigForeignClass");

        fn check_subst<'a, FT: Fn(&str) -> Option<&'a RustType>>(
            generic: &syn::Generics,
            from_ty_name: &str,
            to_ty_name: &str,
            ty_check_name: &str,
            expect_to_ty_name: &str,
            map_others: FT,
        ) -> RustType {
            println!(
                "check_subst: conv {} -> {} with {}",
                from_ty_name, to_ty_name, ty_check_name
            );
            let ret_ty: RustType = GenericTypeConv::simple_new(
                str_to_ty(from_ty_name),
                str_to_ty(to_ty_name),
                generic.clone(),
            )
            .is_conv_possible(&str_to_ty(ty_check_name).into(), None, map_others)
            .expect("check subst failed");
            assert_eq!(
                ret_ty.normalized_name,
                normalize_ty_lifetimes(&str_to_ty(expect_to_ty_name))
            );

            ret_ty
        }

        let pair_generic = get_generic_params_from_code! {
            impl<T1: SwigForeignClass, T2: SwigForeignClass> SwigFrom<(T1, T2)> for CRustObjectPair {
                fn swig_from((x1, x2): (T1, T2)) -> Self {
                    unimplemented!();
                }
            }
        };

        let one_spec = RustType::new(str_to_ty("One"), "One").implements("SwigForeignClass");
        let two_spec = RustType::new(str_to_ty("One"), "One").implements("SwigForeignClass");
        check_subst(
            &pair_generic,
            "(T1, T2)",
            "CRustObjectPair",
            "(One, Two)",
            "CRustObjectPair",
            |name| {
                println!("test pair map, check name {:?}", name);
                if name == "One" {
                    Some(&one_spec)
                } else if name == "Two" {
                    Some(&two_spec)
                } else {
                    None
                }
            },
        );

        check_subst(
            &generic,
            "Rc<T>",
            "jlong",
            "Rc<RefCell<Foo>>",
            "jlong",
            |name| {
                println!("test rt map, check name {:?}", name);
                if name == "Foo" {
                    Some(&foo_spec)
                } else if name == "RefCell < Foo >" {
                    Some(&refcell_foo_spec)
                } else {
                    None
                }
            },
        );

        check_subst(
            &generic,
            "Vec<T>",
            "jobjectArray",
            "Vec<Foo>",
            "jobjectArray",
            |name| {
                if name == "Foo" {
                    Some(&foo_spec)
                } else {
                    None
                }
            },
        );

        let generic = get_generic_params_from_code! {
            impl<'a, T> SwigFrom<&'a RefCell<T>> for RefMut<'a, T> {
                fn swig_from(m: &'a RefCell<T>, _: *mut JNIEnv) -> RefMut<'a, T> {
                    m.borrow_mut()
                }
            }
        };

        check_subst(
            &generic,
            "&RefCell<T>",
            "RefMut<T>",
            "&RefCell<Foo>",
            "RefMut<Foo>",
            |_| None,
        );

        check_subst(
            &generic,
            "&Rc<T>",
            "&T",
            "&Rc<RefCell<Foo>>",
            "&RefCell<Foo>",
            |_| None,
        );

        check_subst(
            &generic,
            "Arc<Mutex<T>>",
            "&Mutex<T>",
            "Arc<Mutex<Foo>>",
            "&Mutex<Foo>",
            |_| None,
        );

        let mutex_guard_foo = check_subst(
            &generic,
            "&Mutex<T>",
            "MutexGuard<T>",
            "&Mutex<Foo>",
            "MutexGuard<Foo>",
            |_| None,
        );
        assert_eq!(
            &*GenericTypeConv::simple_new(
                str_to_ty("MutexGuard<T>"),
                str_to_ty("&T"),
                generic.clone(),
            )
            .is_conv_possible(&mutex_guard_foo, None, |name| if name == "Foo" {
                Some(&foo_spec)
            } else {
                None
            })
            .unwrap()
            .normalized_name,
            "& Foo"
        );

        let box_foo: RustType = str_to_ty("Box<Foo>").into();

        assert_eq!(
            &*GenericTypeConv::simple_new(str_to_ty("jlong"), str_to_ty("Box<T>"), generic,)
                .is_conv_possible(&str_to_ty("jlong").into(), Some(&box_foo), |_| None)
                .unwrap()
                .normalized_name,
            "Box < Foo >"
        );

        let generic = get_generic_params_from_code! {
            impl<T: SwigForeignClass> SwigFrom<Box<T>> for jlong {
                fn swig_from(x: Box<T>, _: *mut JNIEnv) -> jlong {
                    unimplemented!();
                }
            }
        };
        check_subst(&generic, "T", "Box<T>", "Foo", "Box<Foo>", |name| {
            if name == "Foo" {
                Some(&foo_spec)
            } else {
                None
            }
        });

        let generic = get_generic_params_from_code! {
            impl<T, E> SwigFrom<Result<T,E>> for T {
                fn swig_from(v: Result<T, E>, _: *mut JNIEnv) -> T {
                    unimplemented!();
                }
            }
        };
        check_subst(
            &generic,
            "Result<T, E>",
            "T",
            "Result<u8, &'static str>",
            "u8",
            |_| None,
        );
    }

    #[test]
    fn test_get_trait_bounds() {
        let _ = env_logger::try_init();

        assert_eq!(
            get_trait_bounds(&get_generic_params_from_code! {
                impl<T> Foo for Boo {}
            }),
            GenericTraitBoundVec::new(),
        );

        let moo_path: syn::Path = parse_quote! { Moo };

        assert_eq!(
            get_trait_bounds(&get_generic_params_from_code! {
                impl<T: Moo> Foo for Boo {}
            }),
            {
                let mut trait_names = TraitNamesSet::default();
                trait_names.insert(&moo_path);
                let v: GenericTraitBoundVec = smallvec![GenericTraitBound {
                    ty_param: TyParamRef::Own(Ident::new("T", Span::call_site())),
                    trait_names,
                }];
                v
            }
        );

        assert_eq!(
            get_trait_bounds(&get_generic_params_from_code! {
                impl<T> Foo for Boo where T: Moo {}
            }),
            {
                let mut trait_names = TraitNamesSet::default();
                trait_names.insert(&moo_path);
                let v: GenericTraitBoundVec = smallvec![GenericTraitBound {
                    ty_param: TyParamRef::Own(Ident::new("T", Span::call_site())),
                    trait_names,
                }];
                v
            }
        );
    }

    #[test]
    fn test_if_type_slice_return_elem_type() {
        let ty: Type = parse_quote! {
            &[i32]
        };
        let elem_ty: Type = parse_quote! { i32 };
        assert_eq!(
            elem_ty,
            *if_type_slice_return_elem_type(&ty, false).unwrap()
        );

        assert!(if_type_slice_return_elem_type(&elem_ty, false).is_none());
    }

    #[test]
    fn test_work_with_option() {
        assert_eq!(
            "String",
            normalize_ty_lifetimes(
                &if_option_return_some_type(&str_to_ty("Option<String>")).unwrap()
            )
        );
    }

    #[test]
    fn test_work_with_result() {
        assert_eq!(
            if_result_return_ok_err_types(&str_to_ty("Result<bool, String>"))
                .map(|(x, y)| (normalize_ty_lifetimes(&x), normalize_ty_lifetimes(&y)))
                .unwrap(),
            ("bool", "String")
        );
    }

    #[test]
    fn test_work_with_vec() {
        assert_eq!(
            "bool",
            if_vec_return_elem_type(&str_to_ty("Vec<bool>"))
                .map(|x| normalize_ty_lifetimes(&x))
                .unwrap(),
        );
    }

    #[test]
    fn test_work_with_rc() {
        let ty = check_if_smart_pointer_return_inner_type(&str_to_ty("Rc<RefCell<bool>>"), "Rc")
            .unwrap();
        assert_eq!("RefCell < bool >", normalize_ty_lifetimes(&ty));

        let generic_params: syn::Generics = parse_quote! { <T> };
        assert_eq!(
            "bool",
            GenericTypeConv::simple_new(str_to_ty("RefCell<T>"), str_to_ty("T"), generic_params,)
                .is_conv_possible(&ty.into(), None, |_| None)
                .unwrap()
                .normalized_name
        );
    }

    #[test]
    fn test_replace_all_types_with() {
        let t_ident: Ident = parse_quote! { T };
        let e_ident: Ident = parse_quote! { E };
        assert_eq!(
            {
                let ty: Type = parse_quote! { & Vec<T> };
                ty
            },
            replace_all_types_with(&parse_quote! { &T }, &{
                let mut subst_map = TyParamsSubstMap::default();
                subst_map.insert(&t_ident, Some(parse_quote! { Vec<T> }));
                subst_map
            })
        );

        assert_eq!(
            {
                let ty: Type = parse_quote! { Result<i32, String> };
                ty
            },
            replace_all_types_with(&parse_quote! { Result<T, E> }, &{
                let mut subst_map = TyParamsSubstMap::default();
                subst_map.insert(&t_ident, Some(parse_quote! { i32 }));
                subst_map.insert(&e_ident, Some(parse_quote! { String }));
                subst_map
            })
        );
    }

    #[test]
    fn test_list_lifetimes() {
        let my_list_lifetimes = |code| -> Vec<String> {
            let ret = list_lifetimes(&str_to_ty(code));
            ret.iter().map(|v| v.as_str().to_string()).collect()
        };
        assert_eq!(vec!["'a"], my_list_lifetimes("Rc<RefCell<Foo<'a>>>"));
    }

    fn str_to_ty(code: &str) -> syn::Type {
        syn::parse_str::<syn::Type>(code).unwrap()
    }
}
