mod subst_map;
#[cfg(test)]
mod tests;

use std::{
    cell::RefCell,
    fmt::Display,
    hash::{Hash, Hasher},
    mem,
    rc::Rc,
};

use log::trace;
use proc_macro2::{Ident, Span, TokenStream};
use quote::ToTokens;
use rustc_hash::FxHashMap;
use smallvec::SmallVec;
use smol_str::SmolStr;
use syn::{
    parse_quote,
    visit::{visit_lifetime, Visit},
    visit_mut::{
        visit_angle_bracketed_generic_arguments_mut, visit_parenthesized_generic_arguments_mut,
        visit_type_mut, visit_type_reference_mut, visit_type_trait_object_mut, VisitMut,
    },
    Type,
};

pub(in crate) use self::subst_map::{TyParamsSubstItem, TyParamsSubstList, TyParamsSubstMap};
use crate::{
    error::SourceIdSpan,
    source_registry::SourceId,
    typemap::{
        ty::{RustType, RustTypeS, TraitNamesSet},
        TypeConvCode,
    },
};

#[derive(Debug)]
pub(crate) struct TypeName {
    pub(crate) typename: SmolStr,
    pub(crate) span: SourceIdSpan,
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

impl Display for TypeName {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::result::Result<(), core::fmt::Error> {
        f.write_str(self.typename.as_str())
    }
}

impl TypeName {
    pub(crate) fn new<S: Into<SmolStr>>(tn: S, span: SourceIdSpan) -> Self {
        TypeName {
            typename: tn.into(),
            span,
        }
    }
    pub(crate) fn from_ident(id: &Ident, src_id: SourceId) -> Self {
        TypeName::new(id.to_string(), (src_id, id.span()))
    }
    #[inline]
    pub(crate) fn as_str(&self) -> &str {
        self.typename.as_str()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct SpannedSmolStr {
    pub sp: Span,
    pub value: SmolStr,
}

impl SpannedSmolStr {
    pub(crate) fn as_str(&self) -> &str {
        self.value.as_str()
    }
}

impl PartialEq for SpannedSmolStr {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl PartialEq<SmolStr> for SpannedSmolStr {
    fn eq(&self, other: &SmolStr) -> bool {
        self.value == *other
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

fn with_normalize_type_cache<T, F: FnOnce(&mut NormalizeTyLifetimesCache) -> T>(f: F) -> T {
    thread_local!(static INTERNER: RefCell<NormalizeTyLifetimesCache> = {
        RefCell::new(NormalizeTyLifetimesCache::new())
    });
    INTERNER.with(|interner| f(&mut *interner.borrow_mut()))
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
    fn visit_path_arguments_mut(&mut self, i: &mut syn::PathArguments) {
        match *i {
            syn::PathArguments::None => {}
            syn::PathArguments::AngleBracketed(ref mut b) => {
                self.visit_angle_bracketed_generic_arguments_mut(b);
                if b.args.is_empty() {
                    *i = syn::PathArguments::None;
                }
            }
            syn::PathArguments::Parenthesized(ref mut b) => {
                visit_parenthesized_generic_arguments_mut(self, b);
            }
        }
    }
}

pub(crate) fn strip_lifetimes(ty: &mut syn::Type) {
    let mut strip_lifetime = StripLifetime;
    strip_lifetime.visit_type_mut(ty);
}

pub(crate) fn normalize_type(ty: &syn::Type) -> &'static str {
    if let Some(cached_str) = with_normalize_type_cache(|cache| cache.get(ty)) {
        return cached_str;
    }

    let mut strip_lifetime = StripLifetime;
    let mut new_ty = ty.clone();
    strip_lifetime.visit_type_mut(&mut new_ty);

    struct StripDynKeyword;
    impl VisitMut for StripDynKeyword {
        fn visit_type_trait_object_mut(&mut self, i: &mut syn::TypeTraitObject) {
            i.dyn_token = None;
            visit_type_trait_object_mut(self, i);
        }
    }
    let mut strip_dyn = StripDynKeyword;
    strip_dyn.visit_type_mut(&mut new_ty);

    let type_str = new_ty.into_token_stream().to_string();

    with_normalize_type_cache(|cache| cache.insert(ty, type_str))
}

#[derive(Debug)]
pub(crate) struct GenericTypeConv {
    pub src_id: SourceId,
    pub from_ty: syn::Type,
    pub to_ty: syn::Type,
    pub code: TypeConvCode,
    pub dependency: Rc<RefCell<Option<TokenStream>>>,
    pub generic_params: syn::Generics,
    pub to_foreigner_hint: Option<String>,
    pub from_foreigner_hint: Option<String>,
}

impl GenericTypeConv {
    pub(crate) fn new(
        from_ty: Type,
        to_ty: Type,
        generic_params: syn::Generics,
        code: TypeConvCode,
    ) -> GenericTypeConv {
        GenericTypeConv {
            from_ty,
            to_ty,
            code,
            dependency: Rc::new(RefCell::new(None)),
            generic_params,
            to_foreigner_hint: None,
            from_foreigner_hint: None,
            src_id: SourceId::none(),
        }
    }

    pub(crate) fn is_conv_possible<'a, OtherRustTypes>(
        &self,
        ty: &RustType,
        goal_ty: Option<&RustType>,
        others: OtherRustTypes,
    ) -> Option<(syn::Type, SmolStr)>
    where
        OtherRustTypes: Fn(&str) -> Option<&'a RustType>,
    {
        let mut subst_map = TyParamsSubstMap::default();
        trace!(
            "is_conv_possible: begin generic: {} => from_ty: {} => ty: {}",
            DisplayToTokens(&self.generic_params),
            DisplayToTokens(&self.from_ty),
            ty
        );
        for ty_p in self.generic_params.type_params() {
            subst_map.insert(&ty_p.ident, None);
        }
        if !is_second_subst_of_first(&self.from_ty, &ty.ty, &mut subst_map) {
            return None;
        }
        trace!(
            "is_conv_possible: {} is subst of {}, check trait bounds",
            ty,
            DisplayToTokens(&self.from_ty),
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

                if !is_ty_satisfy_trait_bounds(subst_it.ident, val, &trait_bounds, &others) {
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
                /*
                should be:
                jlong -> Box<T> gool Box<Foo>, T = Foo,
                jlong -> Box<T>, gool Foo, T = Foo
                 */
                if !is_second_subst_of_first(&self.to_ty, &goal_ty.ty, &mut subst_map)
                    && subst_map.len() == 1
                    && subst_map.as_slice()[0].ty.is_none()
                {
                    debug_assert_eq!(1, self.generic_params.type_params().count());
                    if let Some(first_generic_ty) = self.generic_params.type_params().next() {
                        *subst_map
                            .get_mut(&first_generic_ty.ident)
                            .expect("Type should be there") = Some(goal_ty.ty.clone());
                    }
                }
            }
            for subst_it in subst_map.as_slice() {
                if let Some(ref val) = subst_it.ty {
                    if !is_ty_satisfy_trait_bounds(subst_it.ident, val, &trait_bounds, &others) {
                        trace!("is_conv_possible: trait bounds check failed");
                        return None;
                    }
                }
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
            }) = subst_map.as_slice().iter().next().as_ref()
            {
                let val_name = normalize_type(val);
                let foreign_name =
                    (*from_foreigner_hint.as_str()).replace(&key.to_string(), &val_name);
                let clean_from_ty = normalize_type(&self.from_ty);
                if ty.normalized_name
                    != RustTypeS::make_unique_typename(&clean_from_ty, &foreign_name)
                {
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
            }) = subst_map.as_slice().iter().next().as_ref()
            {
                let val_name = normalize_type(val);
                let foreign_name =
                    (*to_foreigner_hint.as_str()).replace(&key.to_string(), &val_name);
                Some(foreign_name)
            } else {
                None
            }
        } else {
            None
        };
        let normalized_name =
            RustTypeS::make_unique_typename_if_need(normalize_type(&to_ty).to_string(), to_suffix)
                .into();
        Some((to_ty, normalized_name))
    }
}

fn is_ty_satisfy_trait_bounds<'a, OtherRustTypes>(
    subst_ident: &syn::Ident,
    val: &syn::Type,
    trait_bounds: &[GenericTraitBound],
    others: &OtherRustTypes,
) -> bool
where
    OtherRustTypes: Fn(&str) -> Option<&'a RustType>,
{
    let traits_bound_not_match = |idx: usize| {
        let requires = &trait_bounds[idx].trait_names;
        let val_name = normalize_type(val);

        others(val_name).map_or(true, |rt| !rt.implements.contains_subset(requires))
    };
    !trait_bounds
        .iter()
        .position(|it| it.ty_param.as_ref() == subst_ident)
        .map_or(false, traits_bound_not_match)
}

/// for example true for Result<T, E> Result<u8, u8>
pub(in crate::typemap) fn is_second_subst_of_first(
    ty1: &Type,
    ty2: &Type,
    subst_map: &mut TyParamsSubstMap,
) -> bool {
    trace!(
        "is_second_substitude_of_first {} vs {}",
        DisplayToTokens(ty1),
        DisplayToTokens(ty2)
    );
    match (ty1, ty2) {
        (
            Type::Path(syn::TypePath { path: ref p1, .. }),
            Type::Path(syn::TypePath { path: ref p2, .. }),
        ) => is_second_substitude_of_first_path(p1, p2, subst_map, ty2),
        (Type::Reference(ref mut_ty1), Type::Reference(ref mut_ty2)) => {
            if mut_ty1.mutability != mut_ty2.mutability {
                trace!("is_second_substitude_of_first mutable not match");
                false
            } else {
                is_second_subst_of_first(&*mut_ty1.elem, &*mut_ty2.elem, subst_map)
            }
        }
        (Type::Ptr(ref ptr_ty1), Type::Ptr(ref ptr_ty2)) => {
            if ptr_ty1.mutability != ptr_ty2.mutability {
                trace!("is_second_substitude_of_first mutable not match");
                false
            } else {
                is_second_subst_of_first(&*ptr_ty1.elem, &*ptr_ty2.elem, subst_map)
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
        (Type::ImplTrait(ref trait1), Type::ImplTrait(ref trait2)) => {
            if trait1.bounds.len() != trait2.bounds.len() {
                trace!(
                    "is_second_subst_of_first: impl Trait, number of traits different: {} vs {}",
                    trait1.bounds.len(),
                    trait2.bounds.len()
                );
                return false;
            }
            for (t1, t2) in trait1.bounds.iter().zip(trait2.bounds.iter()) {
                use syn::TypeParamBound::*;
                match (t1, t2) {
                    (Trait(ref b1), Trait(ref b2)) => {
                        if b1.modifier != b2.modifier {
                            trace!("is_second_subst_of_first: impl Trait, trait bounds modifier mismatch");
                            return false;
                        }
                        if !is_second_substitude_of_first_path(&b1.path, &b2.path, subst_map, ty2) {
                            return false;
                        }
                    }
                    (Lifetime(_), Lifetime(_)) => { /*skip*/ }
                    (Trait(_), Lifetime(_)) => {
                        trace!("is_second_subst_of_first: impl Trait, Trait vs Lifetime");
                        return false;
                    }
                    (Lifetime(_), Trait(_)) => {
                        trace!("is_second_subst_of_first: impl Trait, Lifetime vs Trait");
                        return false;
                    }
                }
            }
            true
        }
        (Type::BareFn(ref fn1), Type::BareFn(ref fn2)) => {
            if fn1.abi != fn2.abi {
                trace!("is_second_subst_of_first: bare fn abi mismatch");
                return false;
            }
            if fn1.unsafety != fn2.unsafety {
                trace!("is_second_subst_of_first: bare fn unsafety mismatch");
                return false;
            }
            if fn1.variadic != fn2.variadic {
                trace!("is_second_subst_of_first: bare fn variadic mismatch");
                return false;
            }
            if fn1.inputs.len() != fn2.inputs.len() {
                trace!("is_second_subst_of_first: bare fn inputs len mismatch");
                return false;
            }
            for (arg1, arg2) in fn1.inputs.iter().zip(fn2.inputs.iter()) {
                if !types_equal_inside_path(&arg1.ty, &arg2.ty, subst_map) {
                    return false;
                }
            }
            if !return_type_match(&fn1.output, &fn2.output, subst_map) {
                return false;
            }

            true
        }
        _ => {
            let ret = ty1 == ty2;
            trace!(
                "is_second_substitude_of_first just check equal {} vs {} => {}",
                DisplayToTokens(ty1),
                DisplayToTokens(ty2),
                ret
            );
            ret
        }
    }
}

fn is_second_substitude_of_first_path(
    p1: &syn::Path,
    p2: &syn::Path,
    subst_map: &mut TyParamsSubstMap,
    ty2: &syn::Type,
) -> bool {
    if p1.segments.len() == 1 {
        if let Some(subst) = subst_map.get_mut(&p1.segments[0].ident) {
            if subst.is_none() {
                *subst = Some(ty2.clone());
                return true;
            }
        }
    }
    if p1.segments.len() != p2.segments.len() {
        trace!("is_second_substitude_of_first: path length not match");
        return false;
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
        if !is_second_subst_of_first_path_args(&s1.arguments, &s2.arguments, subst_map) {
            return false;
        }
    }
    true
}

fn is_second_subst_of_first_path_args(
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
                    "is_second_subst_of_first_path_args: param types len not match {} vs {}",
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
                                "is_second_subst_of_first_path_args: generic args cmp {:?} != {:?}",
                                type_p1,
                                type_p2
                            );
                            return false;
                        } else {
                            continue;
                        }
                    }
                };
                if !types_equal_inside_path(type_p1, type_p2, subst_map) {
                    return false;
                }
            }
            true
        }
        (syn::PathArguments::Parenthesized(ref p1), syn::PathArguments::Parenthesized(ref p2)) => {
            if p1.inputs.len() != p2.inputs.len() {
                trace!(
                    "is_second_subst_of_first_path_args: param types len not match {} vs {}",
                    p1.inputs.len(),
                    p2.inputs.len()
                );
                return false;
            }

            for (type_p1, type_p2) in p1.inputs.iter().zip(p2.inputs.iter()) {
                if !types_equal_inside_path(type_p1, type_p2, subst_map) {
                    return false;
                }
            }
            if !return_type_match(&p1.output, &p2.output, subst_map) {
                return false;
            }

            true
        }
        _ => {
            if p1 != p2 {
                trace!(
                    "is_second_subst_of_first_path_args: p1 != p2 => {} {}",
                    DisplayToTokens(p1),
                    DisplayToTokens(p2)
                );
                false
            } else {
                true
            }
        }
    }
}

fn types_equal_inside_path(
    type_p1: &Type,
    type_p2: &Type,
    subst_map: &mut TyParamsSubstMap,
) -> bool {
    let type_p1_name = normalize_type(type_p1);
    let real_type_p1: Type = if let Some(subst) = subst_map.get_mut(&type_p1_name) {
        match *subst {
            Some(ref x) => (*x).clone(),
            None => {
                *subst = Some(type_p2.clone());
                (*type_p2).clone()
            }
        }
    } else {
        (*type_p1).clone()
    };
    trace!("is_second_subst_of_first_path_args: go deeper");
    if !is_second_subst_of_first(&real_type_p1, type_p2, subst_map) {
        return false;
    }
    true
}

fn return_type_match(
    out1: &syn::ReturnType,
    out2: &syn::ReturnType,
    subst_map: &mut TyParamsSubstMap,
) -> bool {
    match (out1, out2) {
        (syn::ReturnType::Default, syn::ReturnType::Default) => { /*ok*/ }
        (syn::ReturnType::Type(_, ref ret_ty1), syn::ReturnType::Type(_, ref ret_ty2)) => {
            if !types_equal_inside_path(ret_ty1, ret_ty2, subst_map) {
                return false;
            }
        }
        _ => {
            trace!(
                "return_type_match: ret output mismatch {} {}",
                DisplayToTokens(out1),
                DisplayToTokens(out2),
            );
            return false;
        }
    }

    true
}

pub(in crate::typemap) fn replace_all_types_with(
    in_ty: &Type,
    subst_map: &TyParamsSubstMap,
) -> Type {
    struct ReplaceTypes<'a, 'b> {
        subst_map: &'a TyParamsSubstMap<'b>,
    }
    impl<'a, 'b> VisitMut for ReplaceTypes<'a, 'b> {
        fn visit_type_mut(&mut self, t: &mut Type) {
            let ty_name = normalize_type(t);
            if let Some(Some(subst)) = self.subst_map.get(&ty_name) {
                *t = subst.clone();
            } else {
                visit_type_mut(self, t);
            }
        }
    }

    trace!(
        "replace_all_types_with in_ty {}, subst_map {:?}",
        DisplayToTokens(in_ty),
        subst_map
    );

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
                        &normalize_type(bounded_ty),
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

pub(crate) fn if_option_return_some_type(ty: &RustType) -> Option<Type> {
    let generic_params: syn::Generics = parse_quote! { <T> };
    let from_ty: Type = parse_quote! { Option<T> };
    let to_ty: Type = parse_quote! { T };

    GenericTypeConv::new(from_ty, to_ty, generic_params, TypeConvCode::invalid())
        .is_conv_possible(ty, None, |_| None)
        .map(|x| x.0)
}

pub(crate) fn if_result_return_ok_err_types(ty: &RustType) -> Option<(Type, Type)> {
    let from_ty: Type = parse_quote! { Result<T, E> };
    let ok_ty: Type = parse_quote! { T };
    let err_ty: Type = parse_quote! { E };
    let generic_params: syn::Generics = parse_quote! { <T, E> };

    let ok_ty = {
        GenericTypeConv::new(
            from_ty.clone(),
            ok_ty,
            generic_params.clone(),
            TypeConvCode::invalid(),
        )
        .is_conv_possible(ty, None, |_| None)
        .map(|x| x.0)
    }?;

    let err_ty = {
        GenericTypeConv::new(from_ty, err_ty, generic_params, TypeConvCode::invalid())
            .is_conv_possible(ty, None, |_| None)
            .map(|x| x.0)
    }?;
    Some((ok_ty, err_ty))
}

/// Sometimes impossible to use RustType, so separate function
pub(crate) fn if_ty_result_return_ok_type(ty: &Type) -> Option<Type> {
    let result_ty: Type = parse_quote! { Result<T, E> };
    let ok_ty: Type = parse_quote! { T };
    let generic_params: syn::Generics = parse_quote! { <T, E> };

    let mut subst_map = TyParamsSubstMap::default();
    for ty_p in generic_params.type_params() {
        subst_map.insert(&ty_p.ident, None);
    }
    if !is_second_subst_of_first(&result_ty, ty, &mut subst_map) {
        return None;
    }

    let to_ty = replace_all_types_with(&ok_ty, &subst_map);

    Some(to_ty)
}

pub(crate) fn check_if_smart_pointer_return_inner_type(
    ty: &RustType,
    smart_ptr_name: &str,
) -> Option<Type> {
    let generic_params: syn::Generics = parse_quote! { <T> };
    let from_ty: Type =
        syn::parse_str(&format!("{}<T>", smart_ptr_name)).expect("smart pointer parse error");
    let to_ty: Type = parse_quote! { T };

    GenericTypeConv::new(from_ty, to_ty, generic_params, TypeConvCode::invalid())
        .is_conv_possible(ty, None, |_| None)
        .map(|x| x.0)
}

pub(crate) fn list_lifetimes(ty: &Type) -> Vec<&syn::Lifetime> {
    struct CatchLifetimes<'a>(Vec<&'a syn::Lifetime>);
    impl<'ast> Visit<'ast> for CatchLifetimes<'ast> {
        fn visit_lifetime(&mut self, lifetime: &'ast syn::Lifetime) {
            self.0.push(&lifetime);
            visit_lifetime(self, lifetime)
        }
    }
    let mut catch_lifetimes = CatchLifetimes(Vec::new());
    catch_lifetimes.visit_type(ty);
    catch_lifetimes.0
}

pub(crate) struct DisplayToTokens<'a, T: ToTokens>(pub &'a T);

impl<T> Display for DisplayToTokens<'_, T>
where
    T: ToTokens,
{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::result::Result<(), core::fmt::Error> {
        f.write_str(&self.0.into_token_stream().to_string())
    }
}

pub(crate) fn parse_ty_with_given_span(
    type_str: &str,
    span: Span,
) -> std::result::Result<Type, syn::Error> {
    syn::LitStr::new(type_str, span).parse::<syn::Type>()
}
