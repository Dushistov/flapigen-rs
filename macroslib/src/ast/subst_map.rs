use proc_macro2::Ident;
use smallvec::SmallVec;

struct IdentRef<'a>(&'a Ident);

impl<'a> PartialEq<IdentRef<'_>> for Ident {
    fn eq(&self, o: &IdentRef<'_>) -> bool {
        self == o.0
    }
}

#[derive(Debug)]
pub(crate) struct TyParamsSubstItem<'a> {
    pub(crate) ident: &'a Ident,
    pub(crate) ty: Option<syn::Type>,
}

#[derive(Default, Debug)]
pub(crate) struct TyParamsSubstMap<'a> {
    inner: SmallVec<[TyParamsSubstItem<'a>; 10]>,
}

impl<'a> TyParamsSubstMap<'a> {
    pub(crate) fn insert(&mut self, ident: &'a Ident, ty: Option<syn::Type>) {
        match self.inner.iter().position(|it| it.ident == ident) {
            Some(idx) => self.inner[idx].ty = ty,
            None => self.inner.push(TyParamsSubstItem { ident, ty }),
        }
    }
    #[inline]
    pub(crate) fn as_slice(&self) -> &[TyParamsSubstItem] {
        &self.inner
    }
    #[inline]
    pub(crate) fn len(&self) -> usize {
        self.inner.len()
    }

    pub fn get_mut(&mut self, k: &Ident) -> Option<&mut Option<syn::Type>> {
        match self.inner.iter().position(|it| it.ident == k) {
            Some(idx) => Some(&mut self.inner[idx].ty),
            None => None,
        }
    }
    pub fn get_mut_by_str(&mut self, k: &str) -> Option<&mut Option<syn::Type>> {
        match self.inner.iter().position(|it| it.ident == k) {
            Some(idx) => Some(&mut self.inner[idx].ty),
            None => None,
        }
    }
    pub fn get(&self, k: &str) -> Option<&Option<syn::Type>> {
        match self.inner.iter().position(|it| it.ident == k) {
            Some(idx) => Some(&self.inner[idx].ty),
            None => None,
        }
    }
}
