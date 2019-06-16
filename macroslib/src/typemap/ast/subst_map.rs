use proc_macro2::Ident;
use smallvec::SmallVec;

struct IdentRef<'a>(&'a Ident);

impl<'a> PartialEq<IdentRef<'_>> for Ident {
    fn eq(&self, o: &IdentRef<'_>) -> bool {
        self == o.0
    }
}

const MAX_SIZE: usize = 10;

#[derive(Debug, PartialEq)]
pub(crate) struct TyParamsSubstItem<'a> {
    pub(crate) ident: &'a Ident,
    pub(crate) ty: Option<syn::Type>,
}

#[derive(Default, Debug, PartialEq)]
pub(crate) struct TyParamsSubstMap<'a> {
    inner: SmallVec<[TyParamsSubstItem<'a>; MAX_SIZE]>,
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
    pub fn get(&self, k: &Ident) -> Option<Option<&syn::Type>> {
        match self.inner.iter().position(|it| it.ident == k) {
            Some(idx) => Some(self.inner[idx].ty.as_ref()),
            None => None,
        }
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
    pub fn get_by_str(&self, k: &str) -> Option<Option<&syn::Type>> {
        match self.inner.iter().position(|it| it.ident == k) {
            Some(idx) => Some(self.inner[idx].ty.as_ref()),
            None => None,
        }
    }
}

pub(crate) type TyParamsSubstList = SmallVec<[(Ident, Option<syn::Type>); MAX_SIZE]>;

impl<'a> From<TyParamsSubstMap<'a>> for TyParamsSubstList {
    fn from(m: TyParamsSubstMap<'a>) -> Self {
        m.inner
            .into_iter()
            .map(|x| (x.ident.clone(), x.ty))
            .collect()
    }
}

impl<'a> From<&'a [(Ident, Option<syn::Type>)]> for TyParamsSubstMap<'a> {
    fn from(v: &'a [(Ident, Option<syn::Type>)]) -> Self {
        let mut ret = TyParamsSubstMap::default();
        for item in v {
            ret.insert(&item.0, item.1.clone());
        }
        ret
    }
}
