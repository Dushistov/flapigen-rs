mod parse;
#[cfg(test)]
mod tests;

use log::debug;
use petgraph::Direction;
use proc_macro2::{Span, TokenStream};
use rustc_hash::FxHashSet;
use smol_str::SmolStr;
use std::{cell::RefCell, convert::TryInto, fmt::Write, rc::Rc};
use syn::{
    spanned::Spanned,
    visit_mut::{visit_type_mut, VisitMut},
    LitStr, Type,
};

use crate::{
    error::{invalid_src_id_span, DiagnosticError, Result, SourceIdSpan},
    source_registry::SourceId,
    typemap::{
        ast::{
            get_trait_bounds, is_second_subst_of_first, normalize_type, parse_ty_with_given_span,
            DisplayToTokens, GenericTypeConv, SpannedSmolStr, TyParamsSubstMap,
        },
        ty::TraitNamesSet,
        TypeConvCode, UniqueName,
    },
    WRITE_TO_MEM_FAILED_MSG,
};
use parse::CItemsList;

pub(crate) use parse::MacroArgs;

static GENERIC_ALIAS: &str = "generic_alias";
static SWIG_CONCAT_IDENTS: &str = "swig_concat_idents";
static SWIG_I_TYPE: &str = "swig_i_type";
static DEFINE_C_TYPE: &str = "define_c_type";
static SWIG_FROM_RUST_TO_I_TYPE: &str = "swig_from_rust_to_i_type";
static SWIG_FROM_I_TYPE_TO_RUST: &str = "swig_from_i_type_to_rust";
static SWIG_FOREIGN_TO_I_TYPE: &str = "swig_foreign_to_i_type";
static SWIG_FOREIGN_FROM_I_TYPE: &str = "swig_foreign_from_i_type";
static SWIG_F_TYPE: &str = "swig_f_type";
pub(in crate::typemap) static SWIG_SUBST_TYPE: &str = "swig_subst_type";

#[derive(Debug)]
pub(crate) struct TypeMapConvRuleInfo {
    pub src_id: SourceId,
    pub span: Span,
    pub rtype_generics: Option<syn::Generics>,
    pub rtype_left_to_right: Option<RTypeConvRule>,
    pub rtype_right_to_left: Option<RTypeConvRule>,
    pub ftype_left_to_right: Vec<FTypeConvRule>,
    pub ftype_right_to_left: Vec<FTypeConvRule>,
    /// For C++ case it is possible to introduce some C types
    pub c_types: Option<CItems>,
    pub generic_c_types: Option<GenericCItems>,
    pub f_code: Vec<ForeignCode>,
    pub generic_aliases: Vec<GenericAlias>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum CItem {
    Struct(syn::ItemStruct),
    Union(syn::ItemUnion),
    Fn(syn::ItemFn),
    Static(syn::ItemStatic),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct CItems {
    pub header_name: SmolStr,
    pub items: Vec<CItem>,
}

#[derive(Debug, Clone)]
pub(crate) struct ForeignCode {
    pub sp: Span,
    pub module_name: SmolStr,
    pub cfg_option: Option<SpannedSmolStr>,
    pub code: String,
}

#[derive(Debug)]
pub(crate) struct GenericAlias {
    pub alias: syn::Ident,
    pub value: TokenStream,
}

#[derive(Debug, Clone)]
pub(crate) struct ModuleName {
    pub name: SmolStr,
    pub sp: Span,
}

impl PartialEq for ModuleName {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl Eq for ModuleName {}

impl Ord for ModuleName {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.name.cmp(&other.name)
    }
}

impl PartialOrd for ModuleName {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.name.cmp(&other.name))
    }
}

#[derive(Debug)]
pub(crate) struct GenericCItems {
    pub header_name: ModuleName,
    pub types: TokenStream,
}

impl TypeMapConvRuleInfo {
    pub(crate) fn is_empty(&self) -> bool {
        self.rtype_generics.is_none()
            && self.rtype_left_to_right.is_none()
            && self.rtype_right_to_left.is_none()
            && self.ftype_left_to_right.is_empty()
            && self.ftype_right_to_left.is_empty()
            && self.c_types.is_none()
            && self.generic_c_types.is_none()
            && self.f_code.is_empty()
            && self.generic_aliases.is_empty()
    }
    pub(crate) fn if_simple_rtype_ftype_map(
        &self,
    ) -> Option<(&Type, &FTypeName, &[ModuleName], Option<&SpannedSmolStr>)> {
        if self.rtype_right_to_left.is_some()
            || !self.ftype_right_to_left.is_empty()
            || self.ftype_left_to_right.len() > 1
        {
            return None;
        }
        match (
            self.rtype_left_to_right.as_ref(),
            self.ftype_left_to_right.first(),
        ) {
            (
                Some(RTypeConvRule {
                    left_ty: ref r_ty,
                    right_ty: None,
                    code: None,
                }),
                Some(FTypeConvRule {
                    left_right_ty: FTypeLeftRightPair::OnlyLeft(ref f_ty),
                    code: None,
                    ref req_modules,
                    ref unique_prefix,
                    ..
                }),
            ) => Some((r_ty, f_ty, req_modules.as_slice(), unique_prefix.as_ref())),
            _ => None,
        }
    }

    /// it is possible to merge to `TypeMap` without help of language backend
    pub(crate) fn if_simple_rtype_ftype_map_no_lang_backend(
        &self,
    ) -> Option<(&Type, &FTypeName, &[ModuleName], Option<&SpannedSmolStr>)> {
        if !self.f_code.is_empty() || self.c_types.is_some() {
            return None;
        }

        self.if_simple_rtype_ftype_map()
    }

    pub(in crate::typemap) fn contains_data_for_language_backend(&self) -> bool {
        self.is_generic()
            || !self.f_code.is_empty()
            || self.c_types.is_some()
            || self
                .ftype_left_to_right
                .iter()
                .any(|x| x.cfg_option.is_some())
            || self
                .ftype_right_to_left
                .iter()
                .any(|x| x.cfg_option.is_some())
            || self.ftype_left_to_right.len() > 1
            || self.ftype_right_to_left.len() > 1
    }

    pub(crate) fn is_generic(&self) -> bool {
        self.rtype_generics.is_some()
    }

    pub(crate) fn is_ty_subst_of_my_generic_rtype<TraitChecker>(
        &self,
        ty: &Type,
        direction: Direction,
        impl_trait: TraitChecker,
    ) -> Option<TyParamsSubstMap>
    where
        TraitChecker: Fn(&Type, &TraitNamesSet) -> bool,
    {
        assert!(self.is_generic());
        let generic_ty = if let Some((r_type, _, _, _)) = self.if_simple_rtype_ftype_map() {
            r_type
        } else {
            let rule = match direction {
                Direction::Incoming => self.rtype_right_to_left.as_ref(),
                Direction::Outgoing => self.rtype_left_to_right.as_ref(),
            };
            let rule = rule?;
            &rule.left_ty
        };

        let generics = self
            .rtype_generics
            .as_ref()
            .expect("Internal error: should used only for generics");
        let mut subst_map = TyParamsSubstMap::default();
        for ty_p in generics.type_params() {
            subst_map.insert(&ty_p.ident, None);
        }
        if !is_second_subst_of_first(generic_ty, ty, &mut subst_map) {
            return None;
        }
        let bounds = get_trait_bounds(generics);
        for b in &bounds {
            if let Some(Some(ty)) = subst_map.get(b.ty_param.as_ref()) {
                if !impl_trait(ty, &b.trait_names) {
                    return None;
                }
            } else {
                println!(
                    "cargo:warning=invalid generic bounds({}) refer unknown parameter, subst. map {:?}",
                    b.ty_param.as_ref(),
                    subst_map
                );
                return None;
            }
        }

        Some(subst_map)
    }

    pub(crate) fn subst_generic_params_to_c_items(
        &self,
        param_map: &TyParamsSubstMap,
        expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
    ) -> Result<Option<CItems>> {
        debug!("subst_generic_params_to_c_items");
        assert!(self.is_generic());
        let type_aliases =
            build_generic_aliases(self.src_id, &self.generic_aliases, param_map, expander)?;
        let mut c_types = self.c_types.clone();
        if let Some(generic_c_types) = self.generic_c_types.as_ref() {
            let code_span = generic_c_types.types.span();
            let code = expand_rust_code(
                &generic_c_types.types.to_string(),
                param_map,
                expander,
                &type_aliases,
                (self.src_id, code_span),
            )?;

            let citems_list: CItemsList =
                syn::LitStr::new(&code, code_span).parse().map_err(|err| {
                    DiagnosticError::new(
                        self.src_id,
                        code_span,
                        format!("can not parse this code after expand: {}", err),
                    )
                    .add_span_note(
                        invalid_src_id_span(),
                        format!("Code after expand: ```\n{}\n```", code),
                    )
                })?;
            assert!(self.c_types.is_none());

            let header_name = expand_module_name(
                &generic_c_types.header_name.name,
                (self.src_id, generic_c_types.header_name.sp),
                &type_aliases,
            )?;

            c_types = Some(CItems {
                header_name,
                items: citems_list.0,
            });
        }
        Ok(c_types)
    }

    pub(crate) fn subst_generic_params(
        &self,
        param_map: TyParamsSubstMap,
        direction: Direction,
        expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
    ) -> Result<Self> {
        debug!(
            "subst_generic_params: direction {:?}, rule {:?}",
            direction, *self
        );
        assert!(self.is_generic());
        let type_aliases =
            build_generic_aliases(self.src_id, &self.generic_aliases, &param_map, expander)?;
        let (rtype_left_to_right, ftype_left_to_right) =
            if direction == Direction::Outgoing || self.if_simple_rtype_ftype_map().is_some() {
                (
                    expand_rtype_rule(
                        self.src_id,
                        self.rtype_left_to_right.as_ref(),
                        &param_map,
                        expander,
                        &type_aliases,
                    )?,
                    expand_ftype_rule(
                        self.src_id,
                        self.ftype_left_to_right.as_ref(),
                        &param_map,
                        expander,
                        &type_aliases,
                    )?,
                )
            } else {
                (None, vec![])
            };
        let (rtype_right_to_left, ftype_right_to_left) = if direction == Direction::Incoming {
            (
                expand_rtype_rule(
                    self.src_id,
                    self.rtype_right_to_left.as_ref(),
                    &param_map,
                    expander,
                    &type_aliases,
                )?,
                expand_ftype_rule(
                    self.src_id,
                    self.ftype_right_to_left.as_ref(),
                    &param_map,
                    expander,
                    &type_aliases,
                )?,
            )
        } else {
            (None, vec![])
        };
        let f_code = expand_fcode(
            &self.f_code,
            &param_map,
            expander,
            &type_aliases,
            self.src_id,
        )?;
        Ok(TypeMapConvRuleInfo {
            src_id: self.src_id,
            span: self.span,
            rtype_generics: None,
            rtype_left_to_right,
            rtype_right_to_left,
            ftype_left_to_right,
            ftype_right_to_left,
            f_code,
            c_types: None,
            generic_c_types: None,
            generic_aliases: vec![],
        })
    }
    pub(in crate::typemap) fn set_src_id(&mut self, src_id: SourceId) {
        self.src_id = src_id;
        fn rtype_change_src_id(r: Option<&mut RTypeConvRule>, src_id: SourceId) {
            if let Some(RTypeConvRule {
                code: Some(ref mut conv_code),
                ..
            }) = r
            {
                conv_code.span.0 = src_id;
            }
        }
        fn ftype_change_src_id(rules: &mut [FTypeConvRule], src_id: SourceId) {
            for r in rules {
                if let Some(conv_code) = r.code.as_mut() {
                    conv_code.span.0 = src_id;
                }
            }
        }

        rtype_change_src_id(self.rtype_left_to_right.as_mut(), src_id);
        rtype_change_src_id(self.rtype_right_to_left.as_mut(), src_id);
        ftype_change_src_id(&mut self.ftype_left_to_right, src_id);
        ftype_change_src_id(&mut self.ftype_right_to_left, src_id);
    }
}

impl TryInto<GenericTypeConv> for TypeMapConvRuleInfo {
    type Error = TypeMapConvRuleInfo;

    fn try_into(mut self) -> std::result::Result<GenericTypeConv, Self::Error> {
        if !self.ftype_left_to_right.is_empty()
            || !self.ftype_right_to_left.is_empty()
            || self.c_types.is_some()
            || self.generic_c_types.is_some()
            || !self.f_code.is_empty()
            || !self.generic_aliases.is_empty()
        {
            return Err(self);
        }

        let generic_params = match self.rtype_generics.take() {
            Some(x) => x,
            None => return Err(self),
        };
        let r_left_to_right = self.rtype_left_to_right.take();
        let r_right_to_left = self.rtype_right_to_left.take();

        let (from_ty, to_ty, code) = match (r_left_to_right, r_right_to_left) {
            (Some(x), Some(y)) => {
                self.rtype_generics = Some(generic_params);
                self.rtype_left_to_right = Some(x);
                self.rtype_right_to_left = Some(y);
                return Err(self);
            }
            (None, None) => {
                self.rtype_generics = Some(generic_params);
                return Err(self);
            }
            (
                Some(RTypeConvRule {
                    left_ty: from_ty,
                    right_ty: Some(to_ty),
                    code: Some(conv_code),
                }),
                None,
            ) => (from_ty, to_ty, conv_code),
            (
                None,
                Some(RTypeConvRule {
                    left_ty: to_ty,
                    right_ty: Some(from_ty),
                    code: Some(conv_code),
                }),
            ) => (from_ty, to_ty, conv_code),
            (Some(x), None) => {
                self.rtype_generics = Some(generic_params);
                self.rtype_left_to_right = Some(x);
                return Err(self);
            }
            (None, Some(y)) => {
                self.rtype_generics = Some(generic_params);
                self.rtype_right_to_left = Some(y);
                return Err(self);
            }
        };

        Ok(GenericTypeConv {
            src_id: self.src_id,
            from_ty,
            to_ty,
            code,
            dependency: Rc::new(RefCell::new(None)),
            generic_params,
            to_foreigner_hint: None,
            from_foreigner_hint: None,
        })
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct RTypeConvRule {
    pub left_ty: Type,
    pub right_ty: Option<Type>,
    pub code: Option<TypeConvCode>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct FTypeConvRule {
    pub req_modules: Vec<ModuleName>,
    pub cfg_option: Option<SpannedSmolStr>,
    pub left_right_ty: FTypeLeftRightPair,
    pub input_to_output: bool,
    pub unique_prefix: Option<SpannedSmolStr>,
    pub code: Option<TypeConvCode>,
}

#[derive(Debug, PartialEq)]
pub(crate) enum FTypeLeftRightPair {
    OnlyLeft(FTypeName),
    OnlyRight(FTypeName),
    Both(FTypeName, FTypeName),
}

impl FTypeLeftRightPair {
    pub(crate) fn span(&self) -> Span {
        use FTypeLeftRightPair::*;
        match self {
            OnlyRight(ref x) => x.sp,
            OnlyLeft(ref x) => x.sp,
            Both(ref x, _) => x.sp,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct FTypeName {
    pub name: UniqueName,
    pub sp: Span,
}

impl PartialEq for FTypeName {
    fn eq(&self, o: &Self) -> bool {
        self.name == o.name
    }
}

impl From<LitStr> for FTypeName {
    fn from(x: LitStr) -> FTypeName {
        FTypeName {
            name: x.value().into(),
            sp: x.span(),
        }
    }
}

pub(crate) struct ExpandedFType {
    pub name: UniqueName,
    pub provided_by_module: Vec<SmolStr>,
}

pub(crate) trait TypeMapConvRuleInfoExpanderHelper {
    fn swig_i_type(&mut self, ty: &syn::Type, opt_arg: Option<&str>) -> Result<syn::Type>;
    fn swig_from_rust_to_i_type(
        &mut self,
        ty: &syn::Type,
        in_var_name: &str,
        out_var_name: &str,
    ) -> Result<String>;
    fn swig_from_i_type_to_rust(
        &mut self,
        ty: &syn::Type,
        in_var_name: &str,
        out_var_name: &str,
    ) -> Result<String>;
    /// param1 - specific for lang backend parameter
    fn swig_f_type(&mut self, ty: &syn::Type, param1: Option<&str>) -> Result<ExpandedFType>;
    fn swig_foreign_to_i_type(&mut self, ty: &syn::Type, var_name: &str) -> Result<String>;
    fn swig_foreign_from_i_type(&mut self, ty: &syn::Type, var_name: &str) -> Result<String>;
}

struct CalcGenericAlias<'a> {
    name: &'a syn::Ident,
    value: syn::Type,
    req_modules: Vec<SmolStr>,
}

fn build_generic_aliases<'a>(
    src_id: SourceId,
    generic_aliases: &'a [GenericAlias],
    param_map: &TyParamsSubstMap,
    expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
) -> Result<Vec<CalcGenericAlias<'a>>> {
    let mut ret = vec![];
    for ga in generic_aliases {
        let item: GenericAliasItem = syn::parse2(ga.value.clone())
            .map_err(|err| DiagnosticError::from_syn_err(src_id, err))?;
        let mut req_modules = Vec::<SmolStr>::new();
        let mut ident = String::new();
        concat_idents(
            src_id,
            item,
            param_map,
            expander,
            &mut req_modules,
            &mut ident,
        )?;
        let ident = properly_escape_str_as_type(&ident);
        let new_type: syn::Type =
            parse_ty_with_given_span(&ident, ga.value.span()).map_err(|err| {
                DiagnosticError::from_syn_err(src_id, err).add_span_note(
                    invalid_src_id_span(),
                    format!("trying to parse '{}' as type", ident),
                )
            })?;
        ret.push(CalcGenericAlias {
            name: &ga.alias,
            value: new_type,
            req_modules,
        });
    }
    Ok(ret)
}

fn properly_escape_str_as_type(s: &str) -> String {
    let data = s
        .replace(":: std :: os :: raw ::", "")
        .replace("std :: os :: raw ::", "");
    let mut ret = String::with_capacity(data.len());
    for ch in data.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            ret.push(ch);
        } else {
            write!(&mut ret, "{}", u32::from(ch)).expect("write to String failed, no free mem?");
        }
    }
    ret
}

#[derive(Debug)]
enum GenericAliasItem {
    Concat(Vec<GenericAliasItem>),
    Ident(syn::Ident),
    SwigIType((syn::Ident, Option<syn::Ident>)),
    SwigFType(syn::Ident),
}

fn concat_idents(
    src_id: SourceId,
    item: GenericAliasItem,
    param_map: &TyParamsSubstMap,
    expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
    req_modules: &mut Vec<SmolStr>,
    ident: &mut String,
) -> Result<()> {
    match item {
        GenericAliasItem::Concat(items) => {
            for it in items {
                concat_idents(src_id, it, param_map, expander, req_modules, ident)?;
            }
        }
        GenericAliasItem::SwigIType((id, opt_arg)) => {
            let ty = find_type_param(param_map, &id.to_string(), (src_id, id.span()))?;
            let i_type: syn::Type = expander.swig_i_type(
                ty.as_ref(),
                opt_arg.as_ref().map(syn::Ident::to_string).as_deref(),
            )?;
            ident.push_str(&DisplayToTokens(&i_type).to_string());
        }
        GenericAliasItem::SwigFType(id) => {
            let ty = find_type_param(param_map, &id.to_string(), (src_id, id.span()))?;
            let mut f_type = expander.swig_f_type(ty.as_ref(), None)?;
            req_modules.append(&mut f_type.provided_by_module);
            ident.push_str(f_type.name.display());
        }
        GenericAliasItem::Ident(id) => ident.push_str(&id.to_string()),
    }
    Ok(())
}

pub(in crate::typemap) fn expand_macroses<E>(code: &str, mut expander: E) -> Result<String>
where
    E: FnMut(/*id*/ &str, /*params*/ Vec<&str>, /*out*/ &mut String) -> Result<()>,
{
    let mut prev_pos = 0;
    let mut ret = String::with_capacity(code.len());
    loop {
        match (code[prev_pos..]).find(char::is_alphabetic) {
            Some(pos) => {
                let skip_chunk = &code[prev_pos..(prev_pos + pos)];
                ret.push_str(skip_chunk);
                prev_pos += pos;
                if let Some((macro_id, params, macro_call_end)) = find_macro(&code[prev_pos..]) {
                    expander(macro_id, params, &mut ret)?;
                    prev_pos += macro_call_end;
                } else {
                    match (code[prev_pos..]).find(|ch: char| !ch.is_alphabetic()) {
                        Some(pos) => {
                            let skip_chunk = &code[prev_pos..(prev_pos + pos)];
                            ret.push_str(skip_chunk);
                            prev_pos += pos;
                        }
                        None => {
                            ret.push_str(&code[prev_pos..]);
                            break;
                        }
                    }
                }
            }
            None => {
                ret.push_str(&code[prev_pos..]);
                break;
            }
        }
    }

    Ok(ret)
}

fn find_macro(code: &str) -> Option<(&str, Vec<&str>, usize)> {
    let id_end = code.find(|ch: char| !(ch.is_alphanumeric() || ch == '_'))?;
    let mut next_pos = id_end + (code[id_end..]).find(|ch: char| !ch.is_whitespace())?;
    if &code[next_pos..=next_pos] != "!" {
        return None;
    }
    next_pos += 1;
    next_pos = next_pos + (code[next_pos..]).find(|ch: char| !ch.is_whitespace())?;
    if &code[next_pos..=next_pos] != "(" {
        return None;
    }
    next_pos += 1;
    let cnt_start = next_pos;
    let mut bracket_counter: usize = 1;
    let mut close_bracket_pos = None;
    for (idx, ch) in code[next_pos..].chars().enumerate() {
        if ch == ')' {
            bracket_counter -= 1;
            if bracket_counter == 0 {
                close_bracket_pos = Some(idx);
                break;
            }
        } else if ch == '(' {
            bracket_counter += 1;
        }
    }
    next_pos += close_bracket_pos?;
    let cnt_end = next_pos;
    let id = &code[0..id_end];
    let params: Vec<&str> = code[cnt_start..cnt_end]
        .trim()
        .split(',')
        .filter_map(|x| {
            let s = x.trim();
            if !s.is_empty() {
                Some(s)
            } else {
                None
            }
        })
        .collect();
    Some((id, params, next_pos + 1))
}

fn expand_rtype_rule(
    src_id: SourceId,
    grule: Option<&RTypeConvRule>,
    param_map: &TyParamsSubstMap,
    expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
    generic_aliases: &[CalcGenericAlias],
) -> Result<Option<RTypeConvRule>> {
    let grule = match grule {
        Some(x) => x,
        None => return Ok(None),
    };
    let left_ty = expand_type(&grule.left_ty, src_id, param_map, expander, generic_aliases)?;
    let right_ty: Option<Type> = match grule.right_ty.as_ref() {
        Some(x) => Some(expand_type(
            x,
            src_id,
            param_map,
            expander,
            generic_aliases,
        )?),
        None => None,
    };

    let code = match grule.code {
        Some(ref x) => {
            let code = expand_rust_code(
                x.as_str(),
                param_map,
                expander,
                generic_aliases,
                (src_id, x.span()),
            )?;
            Some(TypeConvCode::new(code, (SourceId::none(), x.span())))
        }
        None => None,
    };
    Ok(Some(RTypeConvRule {
        left_ty,
        right_ty,
        code,
    }))
}

fn expand_type(
    in_ty: &Type,
    src_id: SourceId,
    subst_map: &TyParamsSubstMap,
    expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
    generic_aliases: &[CalcGenericAlias],
) -> Result<Type> {
    struct ReplaceTypes<'a, 'b, 'c> {
        src_id: SourceId,
        expander: &'a mut dyn TypeMapConvRuleInfoExpanderHelper,
        subst_map: &'a TyParamsSubstMap<'b>,
        generic_aliases: &'a [CalcGenericAlias<'c>],
        err: Option<DiagnosticError>,
    }
    impl VisitMut for ReplaceTypes<'_, '_, '_> {
        fn visit_type_mut(&mut self, t: &mut Type) {
            if self.err.is_some() {
                return;
            }

            let ty_name = normalize_type(t);
            if let Some(Some(subst)) = self.subst_map.get(&ty_name) {
                *t = subst.clone();
            } else {
                visit_type_mut(self, t);
            }

            if let Type::Macro(ref type_macro) = t {
                let ctx_span = (self.src_id, t.span());
                match expand_macro_in_type(
                    type_macro,
                    ctx_span,
                    self.subst_map,
                    self.expander,
                    self.generic_aliases,
                ) {
                    Ok(Some(new_ty)) => *t = new_ty,
                    Ok(None) => {}
                    Err(err) => self.err = Some(err),
                }
            }
        }
    }

    let mut rt = ReplaceTypes {
        subst_map,
        src_id,
        expander,
        generic_aliases,
        err: None,
    };
    let mut new_ty = in_ty.clone();
    rt.visit_type_mut(&mut new_ty);
    Ok(new_ty)
}

fn expand_macro_in_type(
    type_macro: &syn::TypeMacro,
    ctx_span: SourceIdSpan,
    param_map: &TyParamsSubstMap,
    expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
    generic_aliases: &[CalcGenericAlias],
) -> Result<Option<Type>> {
    if type_macro.mac.path.is_ident(SWIG_I_TYPE) {
        let param = type_macro.mac.tokens.to_string();
        let ty = find_type_param(param_map, &param, ctx_span)?;
        let i_type = expander.swig_i_type(ty.as_ref(), None)?;
        Ok(Some(i_type))
    } else {
        let alias_idx = generic_aliases
            .iter()
            .position(|a| type_macro.mac.path.is_ident(a.name))
            .ok_or_else(|| {
                DiagnosticError::new2(
                    ctx_span,
                    format!(
                        "unknown {} name {}",
                        GENERIC_ALIAS,
                        DisplayToTokens(&type_macro.mac.path)
                    ),
                )
            })?;
        Ok(Some(generic_aliases[alias_idx].value.clone()))
    }
}

fn expand_ftype_rule(
    src_id: SourceId,
    grules: &[FTypeConvRule],
    param_map: &TyParamsSubstMap,
    expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
    generic_aliases: &[CalcGenericAlias],
) -> Result<Vec<FTypeConvRule>> {
    let mut ret = Vec::with_capacity(grules.len());

    for grule in grules {
        use FTypeLeftRightPair::*;
        let mut provided_by_module = Vec::<ModuleName>::new();
        let left_right_ty = match grule.left_right_ty {
            OnlyLeft(ref ftype) => OnlyLeft(expand_ftype_name(
                src_id,
                ftype,
                param_map,
                expander,
                generic_aliases,
                &mut provided_by_module,
            )?),
            OnlyRight(ref ftype) => OnlyRight(expand_ftype_name(
                src_id,
                ftype,
                param_map,
                expander,
                generic_aliases,
                &mut provided_by_module,
            )?),
            Both(ref fl, ref fr) => Both(
                expand_ftype_name(
                    src_id,
                    fl,
                    param_map,
                    expander,
                    generic_aliases,
                    &mut provided_by_module,
                )?,
                expand_ftype_name(
                    src_id,
                    fr,
                    param_map,
                    expander,
                    generic_aliases,
                    &mut provided_by_module,
                )?,
            ),
        };
        let code = match grule.code {
            Some(ref x) => {
                let code = expand_foreign_type_conv_code(
                    x,
                    param_map,
                    expander,
                    generic_aliases,
                    (src_id, x.span()),
                )?;
                Some(code)
            }
            None => None,
        };
        for m in &grule.req_modules {
            let mod_name = expand_module_name(&m.name, (src_id, m.sp), generic_aliases)?;
            provided_by_module.push(ModuleName {
                name: mod_name,
                sp: m.sp,
            });
        }
        // preserve order of modules
        let mut mod_uniques = FxHashSet::default();
        provided_by_module.retain(|e| mod_uniques.insert(e.name.clone()));
        let unique_prefix = if let Some(unique_prefix) = grule.unique_prefix.as_ref() {
            let ctx_span = (src_id, unique_prefix.sp);
            let mut provided_by_module = vec![];
            let new_unique_prefix = expand_str_in_ftype_name_context(
                ctx_span,
                unique_prefix.as_str(),
                param_map,
                expander,
                generic_aliases,
                &mut provided_by_module,
            )?;
            Some(SpannedSmolStr {
                sp: unique_prefix.sp,
                value: new_unique_prefix.into(),
            })
        } else {
            None
        };

        ret.push(FTypeConvRule {
            unique_prefix,
            req_modules: provided_by_module,
            cfg_option: grule.cfg_option.clone(),
            left_right_ty,
            input_to_output: grule.input_to_output,
            code,
        });
    }
    Ok(ret)
}

fn call_swig_f_type(
    ctx_sp: SourceIdSpan,
    params: Vec<&str>,
    out: &mut String,
    param_map: &TyParamsSubstMap,
    expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
    generic_aliases: &[CalcGenericAlias],
) -> Result<Vec<SmolStr>> {
    let (type_name, opt_param) = match params.len() {
        1 => (params[0], None),
        2 => (params[0], Some(params[1])),
        _ => {
            return Err(DiagnosticError::new2(
                ctx_sp,
                format!(
                    "{} parameters in {} instead of 1 or 2",
                    params.len(),
                    SWIG_F_TYPE
                ),
            ))
        }
    };
    let ty = if type_name.ends_with("!()") {
        let alias_name = type_name[0..type_name.len() - 3].trim();
        let pos = generic_aliases
            .iter()
            .position(|a| a.name == alias_name)
            .ok_or_else(|| {
                DiagnosticError::new2(ctx_sp, format!("unknown type alias '{}'", alias_name))
            })?;
        TyValueOrRef::Ref(&generic_aliases[pos].value)
    } else {
        find_type_param(param_map, type_name, ctx_sp)?
    };

    let f_type = expander.swig_f_type(ty.as_ref(), opt_param)?;
    out.push_str(f_type.name.value());
    Ok(f_type.provided_by_module)
}

fn expand_ftype_name(
    src_id: SourceId,
    ftype: &FTypeName,
    param_map: &TyParamsSubstMap,
    expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
    generic_aliases: &[CalcGenericAlias],
    provided_by_module: &mut Vec<ModuleName>,
) -> Result<FTypeName> {
    let ctx_span = (src_id, ftype.sp);

    let new_fytpe = expand_str_in_ftype_name_context(
        ctx_span,
        ftype.name.display(),
        param_map,
        expander,
        generic_aliases,
        provided_by_module,
    )?;

    Ok(FTypeName {
        name: new_fytpe.into(),
        sp: ftype.sp,
    })
}

fn expand_str_in_ftype_name_context(
    ctx_span: SourceIdSpan,
    input: &str,
    param_map: &TyParamsSubstMap,
    expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
    generic_aliases: &[CalcGenericAlias],
    provided_by_module: &mut Vec<ModuleName>,
) -> Result<String> {
    expand_macroses(input, |id: &str, params: Vec<&str>, out: &mut String| {
        if id == SWIG_F_TYPE {
            let modules: Vec<SmolStr> =
                call_swig_f_type(ctx_span, params, out, param_map, expander, generic_aliases)?;
            provided_by_module.extend(modules.into_iter().map(|name| ModuleName {
                name,
                sp: Span::call_site(),
            }));
            Ok(())
        } else if id == SWIG_SUBST_TYPE {
            let type_name = if params.len() == 1 {
                params[0]
            } else {
                return Err(DiagnosticError::new2(
                    ctx_span,
                    format!("{} parameters in {} instead of 1", params.len(), id),
                ));
            };
            let ty = find_type_param(param_map, type_name, ctx_span)?;
            out.push_str(normalize_type(ty.as_ref()));
            Ok(())
        } else if let Some(pos) = generic_aliases.iter().position(|a| a.name == id) {
            write!(out, "{}", DisplayToTokens(&generic_aliases[pos].value))
                .expect(WRITE_TO_MEM_FAILED_MSG);
            provided_by_module.extend(generic_aliases[pos].req_modules.iter().map(|name| {
                ModuleName {
                    name: name.clone(),
                    sp: Span::call_site(),
                }
            }));
            Ok(())
        } else {
            Err(DiagnosticError::new2(
                ctx_span,
                format!("unknown macros '{}' in this context", id),
            ))
        }
    })
}

enum TyValueOrRef<'a> {
    Value(Type),
    Ref(&'a Type),
}

impl AsRef<Type> for TyValueOrRef<'_> {
    fn as_ref(&self) -> &Type {
        match self {
            TyValueOrRef::Value(ref ty) => ty,
            TyValueOrRef::Ref(ty) => ty,
        }
    }
}

fn find_type_param<'b>(
    param_map: &'b TyParamsSubstMap,
    param: &str,
    param_span: SourceIdSpan,
) -> Result<TyValueOrRef<'b>> {
    if let Some(Some(ty)) = param_map.get(param) {
        return Ok(TyValueOrRef::Ref(ty));
    }
    let compound_ty: Type = parse_ty_with_given_span(param, param_span.1).map_err(|err| {
        DiagnosticError::new2(param_span, format!("unknown type parameter '{}'", param))
            .add_span_note(invalid_src_id_span(), err)
    })?;
    if let Type::Reference(ty_ref) = compound_ty {
        let param = DisplayToTokens(&ty_ref.elem).to_string();
        if let Some(Some(ty)) = param_map.get(&param) {
            let new_ty = Type::Reference(syn::TypeReference {
                and_token: ty_ref.and_token,
                lifetime: ty_ref.lifetime,
                mutability: ty_ref.mutability,
                elem: Box::new(ty.clone()),
            });
            return Ok(TyValueOrRef::Value(new_ty));
        }
    }
    Err(DiagnosticError::new2(
        param_span,
        format!("unknown type parameter '{}'", param),
    ))
}

fn expand_module_name(
    generic_mod_name: &str,
    ctx_sp: SourceIdSpan,
    aliases: &[CalcGenericAlias],
) -> Result<SmolStr> {
    expand_macroses(
        generic_mod_name,
        |id: &str, params: Vec<&str>, out: &mut String| -> Result<()> {
            if !params.is_empty() {
                Err(DiagnosticError::new2(
                    ctx_sp,
                    format!(
                        "{} ({}) does not accept parameters: {:?}",
                        GENERIC_ALIAS, id, params
                    ),
                ))
            } else if let Some(pos) = aliases.iter().position(|e| e.name == id) {
                write!(out, "{}", DisplayToTokens(&aliases[pos].value))
                    .expect(WRITE_TO_MEM_FAILED_MSG);
                Ok(())
            } else {
                Err(DiagnosticError::new2(
                    ctx_sp,
                    format!("unknown macros '{}' in this context", id),
                ))
            }
        },
    )
    .map(|x| x.into())
}

fn expand_rust_code(
    code: &str,
    param_map: &TyParamsSubstMap,
    expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
    generic_aliases: &[CalcGenericAlias],
    ctx_span: SourceIdSpan,
) -> Result<String> {
    expand_macroses(
        code,
        |id: &str, params: Vec<&str>, out: &mut String| -> Result<()> {
            match id {
                _ if id == SWIG_FROM_RUST_TO_I_TYPE || id == SWIG_FROM_I_TYPE_TO_RUST => {
                    let (type_name, in_var_name, out_var_name) = if params.len() == 3 {
                        (&params[0], &params[1], &params[2])
                    } else {
                        return Err(DiagnosticError::new2(
                            ctx_span,
                            format!("{} parameters in {} instead of 2", params.len(), id),
                        ));
                    };
                    let ty = find_type_param(param_map, type_name, ctx_span)?;
                    let tt: String = if id == SWIG_FROM_RUST_TO_I_TYPE {
                        expander.swig_from_rust_to_i_type(ty.as_ref(), in_var_name, out_var_name)?
                    } else if id == SWIG_FROM_I_TYPE_TO_RUST {
                        expander.swig_from_i_type_to_rust(ty.as_ref(), in_var_name, out_var_name)?
                    } else {
                        unreachable!()
                    };
                    write!(out, "{}", tt).expect(WRITE_TO_MEM_FAILED_MSG);
                }
                _ if id == SWIG_I_TYPE => {
                    let (param, opt_arg) = match params.len() {
                        1 => (params[0], None),
                        2 => (params[0], Some(params[1])),
                        _ => {
                            return Err(DiagnosticError::new2(
                                ctx_span,
                                format!(
                                    "{} parameters in {} instead of 1 or 2",
                                    params.len(),
                                    SWIG_I_TYPE
                                ),
                            ));
                        }
                    };
                    let ty = find_type_param(param_map, param, ctx_span)?;
                    let i_type = expander.swig_i_type(ty.as_ref(), opt_arg)?;
                    write!(out, "{}", normalize_type(&i_type)).expect(WRITE_TO_MEM_FAILED_MSG);
                }
                _ if id == SWIG_SUBST_TYPE => {
                    let type_name = if params.len() == 1 {
                        &params[0]
                    } else {
                        return Err(DiagnosticError::new2(
                            ctx_span,
                            format!("{} parameters in {} instead of 1", params.len(), id),
                        ));
                    };
                    let ty = find_type_param(param_map, type_name, ctx_span)?;
                    write!(out, "{}", normalize_type(ty.as_ref())).expect(WRITE_TO_MEM_FAILED_MSG);
                }
                _ => {
                    if let Some(alias_idx) = generic_aliases.iter().position(|a| a.name == id) {
                        write!(out, "{}", normalize_type(&generic_aliases[alias_idx].value))
                            .expect(WRITE_TO_MEM_FAILED_MSG);
                    } else {
                        write!(out, "{}!(", id).expect(WRITE_TO_MEM_FAILED_MSG);
                        for (i, p) in params.iter().enumerate() {
                            if i == 0 {
                                out.push_str(p);
                            } else {
                                write!(out, ", {}", p).expect(WRITE_TO_MEM_FAILED_MSG);
                            }
                        }
                        out.push(')');
                    }
                }
            }
            Ok(())
        },
    )
}

fn expand_foreign_code(
    code: &str,
    param_map: &TyParamsSubstMap,
    expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
    generic_aliases: &[CalcGenericAlias],
    ctx_span: SourceIdSpan,
) -> Result<String> {
    expand_macroses(
        code,
        |id: &str, params: Vec<&str>, out: &mut String| -> Result<()> {
            match id {
                _ if id == SWIG_F_TYPE => {
                    call_swig_f_type(ctx_span, params, out, param_map, expander, generic_aliases)?;
                }
                _ if id == SWIG_I_TYPE => {
                    let (param, opt_arg) = match params.len() {
                        1 => (params[0], None),
                        2 => (params[0], Some(params[1])),
                        _ => {
                            return Err(DiagnosticError::new2(
                                ctx_span,
                                format!(
                                    "{} parameters in {} instead of 1 or 2",
                                    params.len(),
                                    SWIG_I_TYPE
                                ),
                            ));
                        }
                    };
                    let ty = find_type_param(param_map, param, ctx_span)?;
                    let i_type = expander.swig_i_type(ty.as_ref(), opt_arg)?;
                    let f_type = expander.swig_f_type(&i_type, opt_arg)?;
                    out.push_str(f_type.name.display());
                }
                _ if id == SWIG_FOREIGN_TO_I_TYPE || id == SWIG_FOREIGN_FROM_I_TYPE => {
                    let (type_name, var_name) = if params.len() == 2 {
                        (&params[0], &params[1])
                    } else {
                        return Err(DiagnosticError::new2(
                            ctx_span,
                            format!("{} parameters in {} instead of 2", params.len(), id),
                        ));
                    };

                    let ty = find_type_param(param_map, type_name, ctx_span)?;
                    let tt: String = if id == SWIG_FOREIGN_TO_I_TYPE {
                        expander.swig_foreign_to_i_type(ty.as_ref(), var_name)?
                    } else if id == SWIG_FOREIGN_FROM_I_TYPE {
                        expander.swig_foreign_from_i_type(ty.as_ref(), var_name)?
                    } else {
                        unreachable!()
                    };
                    write!(out, "{}", tt).expect(WRITE_TO_MEM_FAILED_MSG);
                }
                _ => {
                    if let Some(pos) = generic_aliases.iter().position(|a| a.name == id) {
                        write!(out, "{}", DisplayToTokens(&generic_aliases[pos].value))
                            .expect(WRITE_TO_MEM_FAILED_MSG);
                    } else {
                        return Err(DiagnosticError::new2(
                            ctx_span,
                            format!("unknown macro {} in f_type conversion code", id),
                        ));
                    }
                }
            }
            Ok(())
        },
    )
}

fn expand_foreign_type_conv_code(
    code: &TypeConvCode,
    param_map: &TyParamsSubstMap,
    expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
    generic_aliases: &[CalcGenericAlias],
    ctx_span: SourceIdSpan,
) -> Result<TypeConvCode> {
    let ret_code = expand_foreign_code(
        code.as_str(),
        param_map,
        expander,
        generic_aliases,
        ctx_span,
    )?;
    Ok(TypeConvCode::with_params(
        ret_code,
        code.span,
        code.params().to_vec(),
    ))
}

fn expand_fcode(
    f_code: &[ForeignCode],
    param_map: &TyParamsSubstMap,
    expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
    generic_aliases: &[CalcGenericAlias],
    src_id: SourceId,
) -> Result<Vec<ForeignCode>> {
    let mut ret = Vec::<ForeignCode>::with_capacity(f_code.len());
    for fc in f_code {
        let module_name: SmolStr =
            expand_module_name(&fc.module_name, (src_id, fc.sp), generic_aliases)?;
        let code = expand_foreign_code(
            &fc.code,
            param_map,
            expander,
            generic_aliases,
            (src_id, fc.sp),
        )?;
        ret.push(ForeignCode {
            sp: fc.sp,
            module_name,
            cfg_option: fc.cfg_option.clone(),
            code,
        });
    }
    Ok(ret)
}
