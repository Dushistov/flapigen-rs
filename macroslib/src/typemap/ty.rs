use crate::{
    error::DiagnosticError,
    source_registry::SourceId,
    typemap::{ast::strip_lifetimes, RustTypeIdx, TypeConvCode},
};
use proc_macro2::Span;
use quote::ToTokens;
use rustc_hash::FxHashMap;
use smallvec::SmallVec;
use smol_str::SmolStr;
use std::{fmt, ops, rc::Rc};
use syn::spanned::Spanned;

use super::ast::ForeignTypeName;

#[derive(Debug, Clone)]
pub(crate) struct RustTypeS {
    pub src_id: SourceId,
    pub ty: syn::Type,
    pub normalized_name: SmolStr,
    pub implements: ImplementsSet,
    pub(in crate::typemap) graph_idx: RustTypeIdx,
    /// like normalized_name, but _with_ dyn keyword
    typename_without_lifetimes: SmolStr,
}

impl fmt::Display for RustTypeS {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::result::Result<(), core::fmt::Error> {
        write!(f, "{}", self.typename_without_lifetimes)
    }
}

impl RustTypeS {
    pub(in crate::typemap) fn new_without_graph_idx<S>(
        ty: syn::Type,
        norm_name: S,
        src_id: SourceId,
    ) -> RustTypeS
    where
        S: Into<SmolStr>,
    {
        let mut ty_lftms = ty.clone();
        strip_lifetimes(&mut ty_lftms);
        RustTypeS {
            ty,
            normalized_name: norm_name.into(),
            implements: ImplementsSet::default(),
            graph_idx: RustTypeIdx::new(0),
            src_id,
            typename_without_lifetimes: ty_lftms.into_token_stream().to_string().into(),
        }
    }
    #[cfg(test)]
    pub(in crate::typemap) fn implements(mut self, trait_name: &str) -> RustTypeS {
        self.implements.insert(trait_name.into());
        self
    }
    pub(in crate::typemap) fn merge(&mut self, other: &RustTypeS) {
        self.ty = other.ty.clone();
        self.normalized_name = other.normalized_name.clone();
        self.typename_without_lifetimes = other.typename_without_lifetimes.clone();
        self.implements.insert_set(&other.implements);
    }
    pub(crate) fn src_id_span(&self) -> (SourceId, Span) {
        (self.src_id, self.ty.span())
    }
    pub(crate) fn to_idx(&self) -> RustTypeIdx {
        self.graph_idx
    }

    pub(crate) fn make_unique_typename(
        not_unique_name: &str,
        suffix_to_make_unique: &str,
    ) -> String {
        format!("{}{}{}", not_unique_name, 0 as char, suffix_to_make_unique)
    }

    pub(crate) fn make_unique_typename_if_need(
        rust_typename: String,
        suffix: Option<String>,
    ) -> String {
        match suffix {
            Some(s) => RustTypeS::make_unique_typename(&rust_typename, &s),
            None => rust_typename,
        }
    }

    pub(crate) fn typename(&self) -> &str {
        self.typename_without_lifetimes.as_str()
    }

    pub(crate) fn to_type_without_lifetimes(&self) -> syn::Type {
        let mut ty = self.ty.clone();
        strip_lifetimes(&mut ty);
        ty
    }
}

pub(crate) type RustType = Rc<RustTypeS>;

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
            if !self.contains_path(path) {
                return false;
            }
        }
        true
    }
    pub(crate) fn contains_path(&self, path: &syn::Path) -> bool {
        self.inner
            .iter()
            .any(|id: &SmolStr| path.is_ident(id.as_str()))
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
    pub(crate) fn iter(&self) -> impl Iterator<Item = &syn::Path> {
        self.inner.iter().copied()
    }
}

#[derive(Debug)]
pub(crate) struct ForeignTypeS {
    pub name: ForeignTypeName,
    /// specify which foreign module provides this type
    /// it is possible that provided by multiplines modules
    /// for example C++ `std::variant<TypeA, TypeB>
    pub provided_by_module: Vec<SmolStr>,
    pub into_from_rust: Option<ForeignConversionRule>,
    pub from_into_rust: Option<ForeignConversionRule>,
}

impl ForeignTypeS {
    pub(crate) fn src_id_span(&self) -> (SourceId, Span) {
        self.name.span
    }
    pub(crate) fn typename(&self) -> &ForeignTypeName {
        &self.name
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ForeignConversionRule {
    pub(crate) rust_ty: RustTypeIdx,
    pub(crate) intermediate: Option<ForeignConversionIntermediate>,
}

#[derive(Debug, Clone)]
pub(crate) struct ForeignConversionIntermediate {
    pub(crate) input_to_output: bool,
    pub(crate) intermediate_ty: RustTypeIdx,
    pub(crate) conv_code: Rc<TypeConvCode>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct ForeignType(usize);

#[derive(Debug)]
pub(in crate::typemap) struct ForeignTypesStorage {
    ftypes: Vec<ForeignTypeS>,
    name_to_ftype: FxHashMap<SmolStr, ForeignType>,
}

impl ForeignTypesStorage {
    pub(in crate::typemap) fn alloc_new(
        &mut self,
        tn: ForeignTypeName,
        binded_rust_ty: RustTypeIdx,
    ) -> Result<ForeignType, DiagnosticError> {
        let rule = ForeignConversionRule {
            rust_ty: binded_rust_ty,
            intermediate: None,
        };
        self.add_new_ftype(ForeignTypeS {
            name: tn,
            provided_by_module: Vec::new(),
            into_from_rust: Some(rule.clone()),
            from_into_rust: Some(rule),
        })
    }

    pub(in crate::typemap) fn add_new_ftype(
        &mut self,
        ft: ForeignTypeS,
    ) -> Result<ForeignType, DiagnosticError> {
        if let Some(ft2) = self.name_to_ftype.get(ft.name.value()) {
            let mut err = DiagnosticError::new2(
                self.ftypes[ft2.0].name.span,
                format!("Type {} already defined here", ft.name),
            );
            err.span_note(ft.name.span, format!("second mention of type {}", ft.name));
            return Err(err);
        }
        let idx = ForeignType(self.ftypes.len());
        self.ftypes.push(ft);
        self.name_to_ftype
            .insert(self.ftypes[idx.0].name.typename.value_ref().clone(), idx);
        Ok(idx)
    }

    pub(in crate::typemap) fn find_or_alloc(&mut self, ftype_name: ForeignTypeName) -> ForeignType {
        if let Some(ft) = self.name_to_ftype.get(ftype_name.value()) {
            *ft
        } else {
            let ftype = ForeignTypeS {
                name: ftype_name,
                provided_by_module: Vec::new(),
                into_from_rust: None,
                from_into_rust: None,
            };
            self.add_new_ftype(ftype)
                .unwrap_or_else(|err| panic!("Internal error in find_or_alloc_ftype: {}", err))
        }
    }

    pub(in crate::typemap) fn find_ftype_by_name(&self, ftype_name: &str) -> Option<ForeignType> {
        self.name_to_ftype.get(ftype_name).cloned()
    }

    pub(in crate::typemap) fn iter(&self) -> impl Iterator<Item = &ForeignTypeS> {
        self.ftypes.iter()
    }

    pub(in crate::typemap) fn iter_enumerate(
        &self,
    ) -> impl Iterator<Item = (ForeignType, &ForeignTypeS)> {
        self.ftypes
            .iter()
            .enumerate()
            .map(|(idx, item)| (ForeignType(idx), item))
    }

    pub(in crate::typemap) fn into_iter(self) -> impl Iterator<Item = ForeignTypeS> {
        self.ftypes.into_iter()
    }
}

impl Default for ForeignTypesStorage {
    fn default() -> Self {
        ForeignTypesStorage {
            ftypes: Vec::with_capacity(100),
            name_to_ftype: FxHashMap::default(),
        }
    }
}

impl ops::Index<ForeignType> for ForeignTypesStorage {
    type Output = ForeignTypeS;
    fn index(&self, idx: ForeignType) -> &Self::Output {
        &self.ftypes[idx.0]
    }
}

impl ops::IndexMut<ForeignType> for ForeignTypesStorage {
    fn index_mut(&mut self, idx: ForeignType) -> &mut Self::Output {
        &mut self.ftypes[idx.0]
    }
}

impl fmt::Display for ForeignTypesStorage {
    fn fmt(&self, f: &mut fmt::Formatter) -> std::result::Result<(), fmt::Error> {
        writeln!(f, "Foreign types begin")?;
        for item in self.iter() {
            f.write_str(item.name.display())?;
        }
        writeln!(f, "Foreign types end")
    }
}
