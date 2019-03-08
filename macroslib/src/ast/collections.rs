use proc_macro2::Ident;
use smallvec::SmallVec;
#[derive(Default, Debug, Clone)]
pub(crate) struct ImplementsSet {
    inner: SmallVec<[Ident; 5]>,
}

impl ImplementsSet {
    pub(crate) fn insert(&mut self, x: Ident) {
        if !self.inner.iter().any(|it| *it == x) {
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
                .any(|id: &Ident| path.is_ident(IdentRef(id)))
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

struct IdentRef<'a>(&'a Ident);

impl<'a> PartialEq<IdentRef<'_>> for Ident {
    fn eq(&self, o: &IdentRef<'_>) -> bool {
        self == o.0
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
