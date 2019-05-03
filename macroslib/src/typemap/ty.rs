use crate::typemap::ast::normalize_ty_lifetimes;
use smallvec::SmallVec;
use smol_str::SmolStr;
use std::fmt::Display;

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

impl RustType {
    pub(in crate::typemap) fn new<S>(ty: syn::Type, norm_name: S) -> RustType
    where
        S: Into<SmolStr>,
    {
        RustType {
            ty,
            normalized_name: norm_name.into(),
            implements: ImplementsSet::default(),
        }
    }
    pub(in crate::typemap) fn implements(mut self, trait_name: &str) -> RustType {
        self.implements.insert(trait_name.into());
        self
    }
    pub(crate) fn merge(&mut self, other: &RustType) {
        self.ty = other.ty.clone();
        self.normalized_name = other.normalized_name.clone();
        self.implements.insert_set(&other.implements);
    }
    pub(in crate::typemap) fn new_from_type(ty: &syn::Type) -> RustType {
        let normalized_name = normalize_ty_lifetimes(ty);
        RustType::new(ty.clone(), normalized_name)
    }
}

#[derive(Default, Debug, Clone)]
pub(crate) struct ImplementsSet {
    inner: SmallVec<[SmolStr; 5]>,
}

impl ImplementsSet {
    pub(crate) fn insert(&mut self, x: SmolStr) {
        if !self.inner.iter().any(|it| x == *it) {
            self.inner.push(x);
        }
    }
    pub(crate) fn insert_set(&mut self, o: &ImplementsSet) {
        for it in &o.inner {
            self.insert(it.clone());
        }
    }
    pub(crate) fn contains_subset(&self, subset: &TraitNamesSet) -> bool {
        for path in &subset.inner {
            if !self
                .inner
                .iter()
                .any(|id: &SmolStr| path.is_ident(id.as_str()))
            {
                return false;
            }
        }
        true
    }
    pub(crate) fn contains(&self, trait_name: &str) -> bool {
        self.inner.iter().any(|it| *it == trait_name)
    }
}

#[derive(Debug, Default, PartialEq)]
pub(crate) struct TraitNamesSet<'a> {
    inner: SmallVec<[&'a syn::Path; 10]>,
}

impl<'a> TraitNamesSet<'a> {
    pub(crate) fn insert<'b>(&mut self, path: &'b syn::Path)
    where
        'b: 'a,
    {
        if !self.inner.iter().any(|it| **it == *path) {
            self.inner.push(path);
        }
    }
    #[inline]
    pub(crate) fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }
}
