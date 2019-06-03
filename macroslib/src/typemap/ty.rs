use crate::{
    error::DiagnosticError,
    source_registry::SourceId,
    typemap::{ast::TypeName, RustTypeIdx, FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE},
};
use proc_macro2::Span;
use rustc_hash::FxHashMap;
use smallvec::SmallVec;
use smol_str::SmolStr;
use std::{fmt, ops, rc::Rc};
use syn::spanned::Spanned;

#[derive(Debug, Clone)]
pub(crate) struct RustTypeS {
    pub src_id: SourceId,
    pub ty: syn::Type,
    pub normalized_name: SmolStr,
    pub implements: ImplementsSet,
    pub(in crate::typemap) graph_idx: RustTypeIdx,
}

impl fmt::Display for RustTypeS {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::result::Result<(), core::fmt::Error> {
        write!(f, "{}", self.normalized_name)
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
        RustTypeS {
            ty,
            normalized_name: norm_name.into(),
            implements: ImplementsSet::default(),
            graph_idx: RustTypeIdx::new(0),
            src_id,
        }
    }
    pub(in crate::typemap) fn implements(mut self, trait_name: &str) -> RustTypeS {
        self.implements.insert(trait_name.into());
        self
    }
    pub(in crate::typemap) fn merge(&mut self, other: &RustTypeS) {
        self.ty = other.ty.clone();
        self.normalized_name = other.normalized_name.clone();
        self.implements.insert_set(&other.implements);
    }
    pub(crate) fn src_id_span(&self) -> (SourceId, Span) {
        (self.src_id, self.ty.span())
    }
    pub(crate) fn to_idx(&self) -> RustTypeIdx {
        self.graph_idx
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

#[derive(Debug)]
pub(crate) struct ForeignTypeS {
    pub(crate) name: TypeName,
    /// specify which foreign module provides this type
    /// it is possible that provided by multiplines modules
    /// for example C++ `std::variant<TypeA, TypeB>
    pub(crate) provides_by_module: Vec<SmolStr>,
    pub(crate) into_from_rust: Option<ForeignConversationRule>,
    pub(crate) from_into_rust: Option<ForeignConversationRule>,
}

impl ForeignTypeS {
    pub(crate) fn src_id_span(&self) -> (SourceId, Span) {
        self.name.span
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ForeignConversationRule {
    pub(crate) rust_ty: RustTypeIdx,
    pub(crate) intermediate: Option<ForeignConversationIntermediate>,
}

#[derive(Debug, Clone)]
pub(crate) struct ForeignConversationIntermediate {
    pub(crate) intermediate_ty: RustTypeIdx,
    pub(crate) conv_code: FTypeConvCode,
}

#[derive(Debug, Clone)]
pub(crate) struct FTypeConvCode {
    span: Span,
    code: String,
}

impl PartialEq for FTypeConvCode {
    fn eq(&self, o: &Self) -> bool {
        self.code == o.code
    }
}

impl FTypeConvCode {
    /// # Panics
    pub(crate) fn new<S: Into<String>>(code: S, span: Span) -> FTypeConvCode {
        let code: String = code.into();
        assert!(
            code.contains(TO_VAR_TEMPLATE) || code.contains(FROM_VAR_TEMPLATE),
            "code: '{}'",
            code
        );
        FTypeConvCode { code, span }
    }
    /// # Panics
    pub(crate) fn new2<S: Into<String>>(code: S, span: Span) -> FTypeConvCode {
        let code: String = code.into();
        assert!(
            code.contains(TO_VAR_TEMPLATE) && code.contains(FROM_VAR_TEMPLATE),
            "code: '{}'",
            code
        );
        FTypeConvCode { code, span }
    }
}

impl ToString for FTypeConvCode {
    fn to_string(&self) -> String {
        self.code.clone()
    }
}

impl From<FTypeConvCode> for String {
    fn from(x: FTypeConvCode) -> Self {
        x.code
    }
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
        tn: TypeName,
        binded_rust_ty: RustTypeIdx,
    ) -> Result<ForeignType, DiagnosticError> {
        if let Some(ft) = self.name_to_ftype.get(tn.as_str()) {
            let mut err = DiagnosticError::new2(
                self.ftypes[ft.0].name.span,
                format!("Type {} already defined here", tn),
            );
            err.span_note(tn.span, format!("second mention of type {}", tn));
            return Err(err);
        }

        let rule = ForeignConversationRule {
            rust_ty: binded_rust_ty,
            intermediate: None,
        };
        let idx = self.add_new_ftype(ForeignTypeS {
            name: tn,
            provides_by_module: Vec::new(),
            into_from_rust: Some(rule.clone()),
            from_into_rust: Some(rule),
        });
        Ok(idx)
    }

    pub(in crate::typemap) fn add_new_ftype(&mut self, ft: ForeignTypeS) -> ForeignType {
        assert!(self.name_to_ftype.get(ft.name.as_str()).is_none());
        let idx = ForeignType(self.ftypes.len());
        self.ftypes.push(ft);
        self.name_to_ftype
            .insert(self.ftypes[idx.0].name.typename.clone(), idx);
        idx
    }

    pub(in crate::typemap) fn find_or_alloc(&mut self, ftype_name: TypeName) -> ForeignType {
        if let Some(ft) = self.name_to_ftype.get(ftype_name.as_str()) {
            *ft
        } else {
            let idx = ForeignType(self.ftypes.len());
            self.ftypes.push(ForeignTypeS {
                name: ftype_name,
                provides_by_module: Vec::new(),
                into_from_rust: None,
                from_into_rust: None,
            });
            self.name_to_ftype
                .insert(self.ftypes[idx.0].name.typename.clone(), idx);
            idx
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
            writeln!(f, "{}", item.name.as_str())?;
        }
        writeln!(f, "Foreign types end")
    }
}
