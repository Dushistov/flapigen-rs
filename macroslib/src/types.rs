use proc_macro2::{Ident, Span, TokenStream};
use quote::quote;
use smol_str::SmolStr;
use std::fmt;
use syn::{parse_quote, spanned::Spanned, Type};

use crate::{
    error::{DiagnosticError, Result, SourceIdSpan},
    source_registry::SourceId,
    typemap::ast::DisplayToTokens,
    SMART_PTR_COPY_TRAIT,
};

#[derive(Debug, Clone)]
pub(crate) struct ForeignClassInfo {
    pub src_id: SourceId,
    pub name: Ident,
    pub methods: Vec<ForeignMethod>,
    pub self_desc: Option<SelfTypeDesc>,
    pub foreign_code: String,
    pub doc_comments: Vec<String>,
    pub derive_list: Vec<String>,
}

/// Two types instead of one, to simplify live to developer
/// For example, it is possible to use `Rc<RefCell<T>>` as constructor
/// return type, and `T` as self type, and we generate all code to convert
/// back and forth pointer to `RefCell<T>>` and `T`
#[derive(Debug, Clone)]
pub(crate) struct SelfTypeDesc {
    pub self_type: Type,
    pub constructor_ret_type: Type,
}

impl ForeignClassInfo {
    pub(crate) fn span(&self) -> Span {
        self.name.span()
    }
    pub(crate) fn self_type_as_ty(&self) -> Type {
        self.self_desc
            .as_ref()
            .map(|x| x.self_type.clone())
            .unwrap_or_else(|| parse_quote! { () })
    }
    /// common for several language binding generator code
    pub(crate) fn validate_class(&self) -> Result<()> {
        let mut has_constructor = false;
        let mut has_methods = false;
        let mut has_static_methods = false;
        for x in &self.methods {
            match x.variant {
                MethodVariant::Constructor => has_constructor = true,
                MethodVariant::Method(_) => has_methods = true,
                MethodVariant::StaticMethod => has_static_methods = true,
            }
        }
        let self_type_is_some = self.self_desc.is_some();
        if !self_type_is_some && has_methods {
            Err(DiagnosticError::new(
                self.src_id,
                self.span(),
                format!("class {} has methods, but no self_type defined", self.name),
            ))
        } else if self_type_is_some && !has_static_methods && !has_constructor && !has_methods {
            Err(DiagnosticError::new(
                self.src_id,
                self.span(),
                format!(
                    "class {} has only self_type, but no methods or constructors",
                    self.name
                ),
            ))
        } else {
            Ok(())
        }
    }
    pub fn copy_derived(&self) -> bool {
        self.derive_list.iter().any(|x| x == "Copy")
    }
    pub fn smart_ptr_copy_derived(&self) -> bool {
        self.derive_list.iter().any(|x| x == SMART_PTR_COPY_TRAIT)
    }
    /// constructor type implements Clone trait
    pub fn clone_derived(&self) -> bool {
        self.derive_list.iter().any(|x| x == "Clone")
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ForeignMethod {
    pub(crate) variant: MethodVariant,
    pub(crate) rust_id: syn::Path,
    pub(crate) fn_decl: FnDecl,
    pub(crate) name_alias: Option<Ident>,
    pub(crate) access: MethodAccess,
    pub(crate) doc_comments: Vec<String>,
    pub(crate) inline_block: Option<syn::Block>,
    pub(crate) unknown_attrs: Vec<String>,
}

#[derive(Debug, Clone)]
pub(crate) enum FnArg {
    SelfArg(Span, SelfTypeVariant),
    Default(NamedArg),
}

impl FnArg {
    pub(crate) fn as_named_arg(&self) -> syn::Result<&NamedArg> {
        match self {
            FnArg::SelfArg(sp, _) => Err(syn::Error::new(*sp, "expect not self argument here")),
            FnArg::Default(ref arg) => Ok(arg),
        }
    }
    pub(crate) fn as_self_arg(&self, src_id: SourceId) -> Result<SelfTypeVariant> {
        match self {
            FnArg::SelfArg(_, var) => Ok(*var),
            FnArg::Default(ref arg) => Err(DiagnosticError::new(
                src_id,
                arg.span,
                "expect self argument here",
            )),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct NamedArg {
    pub name: SmolStr,
    pub span: Span,
    pub ty: syn::Type,
}

#[derive(Debug, Clone)]
pub(crate) struct FnDecl {
    pub(crate) inputs: Vec<FnArg>,
    pub(crate) output: syn::ReturnType,
}

impl ForeignMethod {
    pub(crate) fn short_name(&self) -> String {
        if let Some(ref name) = self.name_alias {
            name.to_string()
        } else {
            match self.rust_id.segments.len() {
                0 => String::new(),
                n => self.rust_id.segments[n - 1].ident.to_string(),
            }
        }
    }

    pub(crate) fn span(&self) -> Span {
        self.rust_id.span()
    }

    pub(crate) fn is_dummy_constructor(&self) -> bool {
        self.rust_id.segments.is_empty()
    }

    pub(crate) fn arg_names_without_self(&self) -> impl Iterator<Item = &str> {
        let skip = match self.variant {
            MethodVariant::Method(_) => 1,
            _ => 0,
        };
        self.fn_decl
            .inputs
            .iter()
            .skip(skip)
            .map(|x| x.as_named_arg().unwrap().name.as_str())
    }

    pub(crate) fn generate_code_to_call_rust_func(&self) -> String {
        if let Some(ref code_block) = self.inline_block {
            format!("{}", DisplayToTokens(code_block))
        } else {
            let args_names = self
                .arg_names_without_self()
                .fold(String::new(), |mut acc, x| {
                    if !acc.is_empty() {
                        acc.push_str(", ");
                    }
                    acc.push_str(x);
                    acc
                });
            if let MethodVariant::Method(_) = self.variant {
                format!("{}(this, {})", DisplayToTokens(&self.rust_id), args_names)
            } else {
                format!("{}({})", DisplayToTokens(&self.rust_id), args_names)
            }
        }
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub(crate) enum MethodAccess {
    Private,
    Public,
    Protected,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum MethodVariant {
    Constructor,
    Method(SelfTypeVariant),
    StaticMethod,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum SelfTypeVariant {
    RptrMut,
    Rptr,
    Mut,
    Default,
}

impl From<SelfTypeVariant> for TokenStream {
    fn from(x: SelfTypeVariant) -> TokenStream {
        match x {
            SelfTypeVariant::RptrMut => quote!(&mut self),
            SelfTypeVariant::Rptr => quote!(&self),
            SelfTypeVariant::Mut => quote!(mut self),
            SelfTypeVariant::Default => quote!(self),
        }
    }
}

impl fmt::Display for SelfTypeVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> fmt::Result {
        use SelfTypeVariant::*;
        f.write_str(match self {
            RptrMut => "&mut self",
            Rptr => "&self",
            Mut => "mut self",
            Default => "self",
        })
    }
}

impl SelfTypeVariant {
    pub(crate) fn is_read_only(self) -> bool {
        match self {
            SelfTypeVariant::RptrMut | SelfTypeVariant::Mut => false,
            SelfTypeVariant::Default | SelfTypeVariant::Rptr => true,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ForeignEnumInfo {
    pub(crate) src_id: SourceId,
    pub(crate) name: Ident,
    pub(crate) items: Vec<ForeignEnumItem>,
    pub(crate) doc_comments: Vec<String>,
    pub(crate) derive_list: Vec<String>,
}

impl ForeignEnumInfo {
    pub(crate) fn span(&self) -> Span {
        self.name.span()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ForeignEnumItem {
    pub(crate) name: Ident,
    pub(crate) rust_name: syn::Path,
    pub(crate) doc_comments: Vec<String>,
}

pub(crate) struct ForeignInterface {
    pub(crate) src_id: SourceId,
    pub(crate) name: Ident,
    pub(crate) self_type: syn::TypeTraitObject,
    pub(crate) doc_comments: Vec<String>,
    pub(crate) items: Vec<ForeignInterfaceMethod>,
}

impl ForeignInterface {
    pub(crate) fn span(&self) -> Span {
        self.name.span()
    }
    pub(crate) fn src_id_span(&self) -> SourceIdSpan {
        (self.src_id, self.name.span())
    }
}

pub(crate) struct ForeignInterfaceMethod {
    pub(crate) name: Ident,
    pub(crate) rust_name: syn::Path,
    pub(crate) fn_decl: FnDecl,
    pub(crate) doc_comments: Vec<String>,
}

impl ForeignInterfaceMethod {
    pub(crate) fn arg_names_without_self(&self) -> impl Iterator<Item = &str> {
        self.fn_decl
            .inputs
            .iter()
            .skip(1)
            .map(|x| x.as_named_arg().unwrap().name.as_str())
    }
}

pub(crate) enum ItemToExpand {
    Class(Box<ForeignClassInfo>),
    Interface(ForeignInterface),
    Enum(ForeignEnumInfo),
}
