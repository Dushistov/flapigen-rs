mod parse;
#[cfg(test)]
mod tests;

use petgraph::Direction;
use proc_macro2::{Span, TokenStream};
use smol_str::SmolStr;
use std::fmt::Write;
use syn::{spanned::Spanned, LitStr, Type};

use crate::{
    error::{invalid_src_id_span, DiagnosticError, Result, SourceIdSpan},
    source_registry::SourceId,
    typemap::{
        ast::{
            get_trait_bounds, is_second_subst_of_first, parse_ty_with_given_span,
            replace_all_types_with, DisplayToTokens, SpannedSmolStr, TyParamsSubstMap,
        },
        ty::{FTypeConvCode, TraitNamesSet},
    },
    WRITE_TO_MEM_FAILED_MSG,
};
use parse::CItemsList;

static GENERIC_ALIAS: &str = "generic_alias";
static SWIG_CONCAT_IDENTS: &str = "swig_concat_idents";
static SWIG_I_TYPE: &str = "swig_i_type";
static DEFINE_C_TYPE: &str = "define_c_type";
static SWIG_FROM_RUST_TO_I_TYPE: &str = "swig_from_rust_to_i_type";
static SWIG_FROM_I_TYPE_TO_RUST: &str = "swig_from_i_type_to_rust";
static SWIG_FOREIGN_TO_I_TYPE: &str = "swig_foreign_to_i_type";
static SWIG_FOREIGN_FROM_I_TYPE: &str = "swig_foreign_from_i_type";
static SWIG_F_TYPE: &str = "swig_f_type";
static SWIG_SUBST_TYPE: &str = "swig_subst_type";

#[derive(Debug)]
pub(crate) struct TypeMapConvRuleInfo {
    pub src_id: SourceId,
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
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct CItems {
    pub header_name: SmolStr,
    pub items: Vec<CItem>,
}

#[derive(Debug, Clone)]
pub(crate) struct ForeignCode {
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
    pub(crate) fn if_simple_rtype_ftype_map(&self) -> Option<(&Type, &FTypeName, &[ModuleName])> {
        if self.rtype_right_to_left.is_some()
            || !self.ftype_right_to_left.is_empty()
            || self.ftype_left_to_right.len() > 1
            || !self.f_code.is_empty()
            || self.c_types.is_some()
        {
            return None;
        }
        match (
            self.rtype_left_to_right.as_ref(),
            self.ftype_left_to_right.get(0),
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
                    ..
                }),
            ) => Some((r_ty, f_ty, req_modules.as_slice())),
            _ => None,
        }
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
        let rule = match direction {
            Direction::Incoming => self.rtype_right_to_left.as_ref(),
            Direction::Outgoing => self.rtype_left_to_right.as_ref(),
        };
        let rule = rule?;

        let generics = self
            .rtype_generics
            .as_ref()
            .expect("Internal error: should used only for generics");
        let mut subst_map = TyParamsSubstMap::default();
        for ty_p in generics.type_params() {
            subst_map.insert(&ty_p.ident, None);
        }
        if !is_second_subst_of_first(&rule.left_ty, ty, &mut subst_map) {
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
                    "warning=invalid generic bounds({}) refer unknown parameter, subst. map {:?}",
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
        assert!(self.is_generic());
        let type_aliases =
            build_generic_aliases(self.src_id, &self.generic_aliases, &param_map, expander)?;
        let mut c_types = self.c_types.clone();
        if let Some(generic_c_types) = self.generic_c_types.as_ref() {
            let code_span = generic_c_types.types.span();
            let code = expand_macroses(
                &generic_c_types.types.to_string(),
                |id: &str, params: Vec<&str>, out: &mut String| -> Result<()> {
                    if id == SWIG_I_TYPE {
                        let param = if params.len() == 1 {
                            &params[0]
                        } else {
                            return Err(DiagnosticError::new(
                                self.src_id,
                                code_span,
                                format!(
                                    "{} parameters in {} instead of 1",
                                    params.len(),
                                    SWIG_I_TYPE
                                ),
                            ));
                        };
                        let ty = find_type_param(param_map, param, (self.src_id, code_span))?;
                        let i_type = expander.swig_i_type(ty.as_ref())?;
                        write!(out, "{}", DisplayToTokens(&i_type))
                            .expect("write to String failed");
                        Ok(())
                    } else if let Some(pos) = type_aliases
                        .iter()
                        .position(|(ident, _)| ident.to_string() == id)
                    {
                        write!(out, "{}", DisplayToTokens(&type_aliases[pos].1))
                            .expect("write to String failed");
                        Ok(())
                    } else {
                        Err(DiagnosticError::new(
                            self.src_id,
                            code_span,
                            format!("unknown macros '{}' in this context", id),
                        ))
                    }
                },
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
        assert!(self.is_generic());
        let type_aliases =
            build_generic_aliases(self.src_id, &self.generic_aliases, &param_map, expander)?;

        let (rtype_left_to_right, ftype_left_to_right) = if direction == Direction::Outgoing {
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

        Ok(TypeMapConvRuleInfo {
            src_id: self.src_id,
            rtype_generics: None,
            rtype_left_to_right,
            rtype_right_to_left,
            ftype_left_to_right,
            ftype_right_to_left,
            f_code: self.f_code.clone(),
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

#[derive(Debug, PartialEq)]
pub(crate) struct RTypeConvRule {
    pub left_ty: Type,
    pub right_ty: Option<Type>,
    pub code: Option<FTypeConvCode>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct FTypeConvRule {
    pub req_modules: Vec<ModuleName>,
    pub cfg_option: Option<SpannedSmolStr>,
    pub left_right_ty: FTypeLeftRightPair,
    pub code: Option<FTypeConvCode>,
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
    pub name: SmolStr,
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
    pub name: SmolStr,
    pub provides_by_module: Vec<SmolStr>,
}

pub(crate) trait TypeMapConvRuleInfoExpanderHelper {
    fn swig_i_type(&mut self, ty: &syn::Type) -> Result<syn::Type>;
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
    fn swig_f_type(
        &mut self,
        ty: &syn::Type,
        direction: Option<Direction>,
    ) -> Result<ExpandedFType>;
    fn swig_foreign_to_i_type(&mut self, ty: &syn::Type, var_name: &str) -> Result<String>;
    fn swig_foreign_from_i_type(&mut self, ty: &syn::Type, var_name: &str) -> Result<String>;
}

fn build_generic_aliases<'a, 'b>(
    src_id: SourceId,
    generic_aliases: &'a [GenericAlias],
    param_map: &'b TyParamsSubstMap,
    expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
) -> Result<Vec<(&'a syn::Ident, Type)>> {
    let mut ret = vec![];
    for ga in generic_aliases {
        let item: GenericAliasItem = syn::parse2(ga.value.clone())
            .map_err(|err| DiagnosticError::from_syn_err(src_id, err))?;
        let mut ident = String::new();
        concat_idents(src_id, item, param_map, expander, &mut ident)?;
        let ident = properly_escape_str_as_type(&ident);
        let new_type: syn::Type =
            parse_ty_with_given_span(&ident, ga.value.span()).map_err(|err| {
                DiagnosticError::from_syn_err(src_id, err).add_span_note(
                    invalid_src_id_span(),
                    format!("trying to parse '{}' as type", ident),
                )
            })?;
        ret.push((&ga.alias, new_type));
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
    SwigIType(syn::Ident),
}

fn concat_idents(
    src_id: SourceId,
    item: GenericAliasItem,
    param_map: &TyParamsSubstMap,
    expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
    ident: &mut String,
) -> Result<()> {
    match item {
        GenericAliasItem::Concat(items) => {
            for it in items {
                concat_idents(src_id, it, param_map, expander, ident)?;
            }
        }
        GenericAliasItem::SwigIType(id) => {
            let ty = find_type_param(param_map, &id.to_string(), (src_id, id.span()))?;
            let i_type: syn::Type = expander.swig_i_type(ty.as_ref())?;
            ident.push_str(&DisplayToTokens(&i_type).to_string());
        }
        GenericAliasItem::Ident(id) => ident.push_str(&id.to_string()),
    }
    Ok(())
}

fn expand_macroses<'a, E>(code: &str, mut expander: E) -> Result<String>
where
    E: FnMut(&str, Vec<&str>, &mut String) -> Result<()>,
{
    let mut prev_pos = 0;
    let mut ret = String::with_capacity(code.len());
    loop {
        match (&code[prev_pos..]).find(char::is_alphabetic) {
            Some(pos) => {
                let skip_chunk = &code[prev_pos..(prev_pos + pos)];
                ret.push_str(skip_chunk);
                prev_pos += pos;
                if let Some((macro_id, params, macro_call_end)) = find_macro(&code[prev_pos..]) {
                    expander(macro_id, params, &mut ret)?;
                    prev_pos += macro_call_end;
                } else {
                    match (&code[prev_pos..]).find(|ch: char| !ch.is_alphabetic()) {
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
    let mut next_pos = id_end + (&code[id_end..]).find(|ch: char| !ch.is_whitespace())?;
    if &code[next_pos..(next_pos + 1)] != "!" {
        return None;
    }
    next_pos += 1;
    next_pos = next_pos + (&code[next_pos..]).find(|ch: char| !ch.is_whitespace())?;
    if &code[next_pos..(next_pos + 1)] != "(" {
        return None;
    }
    next_pos += 1;
    let cnt_start = next_pos;
    let mut bracket_counter: usize = 1;
    let mut close_bracket_pos = None;
    for (idx, ch) in (&code[next_pos..]).chars().enumerate() {
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
    next_pos = next_pos + close_bracket_pos?;
    let cnt_end = next_pos;
    let id = &code[0..id_end];
    let params: Vec<&str> = (&code[cnt_start..cnt_end])
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
    generic_aliases: &[(&syn::Ident, Type)],
) -> Result<Option<RTypeConvRule>> {
    let grule = match grule {
        Some(x) => x,
        None => return Ok(None),
    };
    let left_ty = replace_all_types_with(&grule.left_ty, param_map);
    let right_ty: Option<Type> = match grule.right_ty.as_ref() {
        Some(x) => match x {
            Type::Macro(ref type_macro) => {
                let alias_idx = generic_aliases
                    .iter()
                    .position(|a| type_macro.mac.path.is_ident(a.0.clone()))
                    .ok_or_else(|| {
                        DiagnosticError::new(
                            src_id,
                            x.span(),
                            format!(
                                "unknown {} name {}",
                                GENERIC_ALIAS,
                                DisplayToTokens(&type_macro.mac.path)
                            ),
                        )
                    })?;
                Some(generic_aliases[alias_idx].1.clone())
            }
            _ => Some(replace_all_types_with(&x, param_map)),
        },
        None => None,
    };

    let code = match grule.code {
        Some(ref x) => {
            let code = expand_macroses(
                x.as_str(),
                |id: &str, params: Vec<&str>, out: &mut String| -> Result<()> {
                    match id {
                        _ if id == SWIG_FROM_RUST_TO_I_TYPE || id == SWIG_FROM_I_TYPE_TO_RUST => {
                            let (type_name, in_var_name, out_var_name) = if params.len() == 3 {
                                (&params[0], &params[1], &params[2])
                            } else {
                                return Err(DiagnosticError::new(
                                    src_id,
                                    x.span(),
                                    format!("{} parameters in {} instead of 2", params.len(), id),
                                ));
                            };
                            let ty = find_type_param(param_map, type_name, (src_id, x.span()))?;
                            let tt: String = if id == SWIG_FROM_RUST_TO_I_TYPE {
                                expander.swig_from_rust_to_i_type(
                                    ty.as_ref(),
                                    in_var_name,
                                    out_var_name,
                                )?
                            } else if id == SWIG_FROM_I_TYPE_TO_RUST {
                                expander.swig_from_i_type_to_rust(
                                    ty.as_ref(),
                                    in_var_name,
                                    out_var_name,
                                )?
                            } else {
                                unreachable!()
                            };
                            write!(out, "{}", tt).expect(WRITE_TO_MEM_FAILED_MSG);
                        }
                        _ if id == SWIG_SUBST_TYPE => {
                            let type_name = if params.len() == 1 {
                                &params[0]
                            } else {
                                return Err(DiagnosticError::new(
                                    src_id,
                                    x.span(),
                                    format!("{} parameters in {} instead of 1", params.len(), id),
                                ));
                            };
                            let ty = find_type_param(param_map, type_name, (src_id, x.span()))?;
                            write!(out, "{}", DisplayToTokens(ty.as_ref()))
                                .expect(WRITE_TO_MEM_FAILED_MSG);
                        }
                        _ => {
                            if let Some(alias_idx) = generic_aliases.iter().position(|a| a.0 == id)
                            {
                                write!(out, "{}", DisplayToTokens(&generic_aliases[alias_idx].1))
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
                                out.push_str(")");
                            }
                        }
                    }
                    Ok(())
                },
            )?;
            Some(FTypeConvCode::new(code, (SourceId::none(), x.span())))
        }
        None => None,
    };

    Ok(Some(RTypeConvRule {
        left_ty,
        right_ty,
        code,
    }))
}

fn expand_ftype_rule(
    src_id: SourceId,
    grules: &[FTypeConvRule],
    param_map: &TyParamsSubstMap,
    expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
    generic_aliases: &[(&syn::Ident, Type)],
) -> Result<Vec<FTypeConvRule>> {
    let mut ret = Vec::with_capacity(grules.len());

    for grule in grules {
        use FTypeLeftRightPair::*;
        let mut provides_by_module = Vec::<ModuleName>::new();
        let left_right_ty = match grule.left_right_ty {
            OnlyLeft(ref ftype) => OnlyLeft(expand_ftype_name(
                src_id,
                ftype,
                param_map,
                expander,
                generic_aliases,
                &mut provides_by_module,
            )?),
            OnlyRight(ref ftype) => OnlyRight(expand_ftype_name(
                src_id,
                ftype,
                param_map,
                expander,
                generic_aliases,
                &mut provides_by_module,
            )?),
            Both(ref fl, ref fr) => Both(
                expand_ftype_name(
                    src_id,
                    fl,
                    param_map,
                    expander,
                    generic_aliases,
                    &mut provides_by_module,
                )?,
                expand_ftype_name(
                    src_id,
                    fr,
                    param_map,
                    expander,
                    generic_aliases,
                    &mut provides_by_module,
                )?,
            ),
        };
        let code = match grule.code {
            Some(ref x) => {
                let code = expand_macroses(
                    x.as_str(),
                    |id: &str, params: Vec<&str>, out: &mut String| -> Result<()> {
                        match id {
                            _ if id == SWIG_F_TYPE => {
                                call_swig_f_type(
                                    src_id,
                                    x.span(),
                                    params,
                                    out,
                                    param_map,
                                    expander,
                                    generic_aliases,
                                )?;
                            }
                            _ if id == SWIG_FOREIGN_TO_I_TYPE || id == SWIG_FOREIGN_FROM_I_TYPE => {
                                let (type_name, var_name) = if params.len() == 2 {
                                    (&params[0], &params[1])
                                } else {
                                    return Err(DiagnosticError::new(
                                        src_id,
                                        x.span(),
                                        format!(
                                            "{} parameters in {} instead of 2",
                                            params.len(),
                                            id
                                        ),
                                    ));
                                };

                                let ty = find_type_param(param_map, type_name, (src_id, x.span()))?;
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
                                return Err(DiagnosticError::new(
                                    src_id,
                                    x.span(),
                                    format!("unknown macro {} in f_type conversation code", id),
                                ))
                            }
                        }
                        Ok(())
                    },
                )?;
                Some(FTypeConvCode::new(code, (SourceId::none(), x.span())))
            }
            None => None,
        };
        for m in &grule.req_modules {
            let mod_name = expand_module_name(&m.name, (src_id, m.sp), generic_aliases)?;
            provides_by_module.push(ModuleName {
                name: mod_name,
                sp: m.sp,
            });
        }
        provides_by_module.sort();
        provides_by_module.dedup();
        ret.push(FTypeConvRule {
            req_modules: provides_by_module,
            cfg_option: grule.cfg_option.clone(),
            left_right_ty,
            code,
        });
    }
    Ok(ret)
}

fn call_swig_f_type(
    src_id: SourceId,
    sp: Span,
    params: Vec<&str>,
    out: &mut String,
    param_map: &TyParamsSubstMap,
    expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
    generic_aliases: &[(&syn::Ident, Type)],
) -> Result<Vec<SmolStr>> {
    let (type_name, direction) = match params.len() {
        1 => (&params[0], None),
        2 => {
            let direction = match params[1] {
                "output" => Direction::Outgoing,
                "input" => Direction::Incoming,
                _ => {
                    return Err(DiagnosticError::new(
                        src_id,
                        sp,
                        format!(
                            "{} parameters in {} instead of 1",
                            params.len(),
                            SWIG_F_TYPE
                        ),
                    ))
                }
            };

            (&params[0], Some(direction))
        }
        _ => {
            return Err(DiagnosticError::new(
                src_id,
                sp,
                format!(
                    "{} parameters in {} instead of 1",
                    params.len(),
                    SWIG_F_TYPE
                ),
            ))
        }
    };
    let ty = if type_name.ends_with("!()") {
        let alias_name = (&type_name[0..type_name.len() - 3]).trim();
        let pos = generic_aliases
            .iter()
            .position(|a| a.0 == alias_name)
            .ok_or_else(|| {
                DiagnosticError::new(src_id, sp, format!("unknown type alias '{}'", alias_name))
            })?;
        TyValueOrRef::Ref(&generic_aliases[pos].1)
    } else {
        find_type_param(param_map, type_name, (src_id, sp))?
    };

    let f_type = expander.swig_f_type(ty.as_ref(), direction)?;
    out.push_str(&f_type.name);
    Ok(f_type.provides_by_module)
}

fn expand_ftype_name(
    src_id: SourceId,
    ftype: &FTypeName,
    param_map: &TyParamsSubstMap,
    expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
    generic_aliases: &[(&syn::Ident, Type)],
    provides_by_module: &mut Vec<ModuleName>,
) -> Result<FTypeName> {
    let new_fytpe = expand_macroses(
        ftype.name.as_str(),
        |id: &str, params: Vec<&str>, out: &mut String| {
            if id == SWIG_F_TYPE {
                let modules: Vec<SmolStr> = call_swig_f_type(
                    src_id,
                    ftype.sp,
                    params,
                    out,
                    param_map,
                    expander,
                    generic_aliases,
                )?;
                provides_by_module.extend(modules.into_iter().map(|name| ModuleName {
                    name,
                    sp: Span::call_site(),
                }));
                Ok(())
            } else {
                Err(DiagnosticError::new(
                    src_id,
                    ftype.sp,
                    format!("unknown macros '{}' in this context", id),
                ))
            }
        },
    )?;
    Ok(FTypeName {
        name: new_fytpe.into(),
        sp: ftype.sp,
    })
}

enum TyValueOrRef<'a> {
    Value(Type),
    Ref(&'a Type),
}

impl<'a> AsRef<Type> for TyValueOrRef<'_> {
    fn as_ref(&self) -> &Type {
        match self {
            TyValueOrRef::Value(ref ty) => ty,
            TyValueOrRef::Ref(ty) => ty,
        }
    }
}

fn find_type_param<'a, 'b>(
    param_map: &'b TyParamsSubstMap,
    param: &'a str,
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
        let param = format!("{}", DisplayToTokens(&ty_ref.elem));
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
    aliases: &[(&syn::Ident, Type)],
) -> Result<SmolStr> {
    expand_macroses(
        generic_mod_name,
        |id: &str, params: Vec<&str>, out: &mut String| -> Result<()> {
            if params.len() != 0 {
                Err(DiagnosticError::new2(
                    ctx_sp,
                    format!(
                        "{} ({}) does not accept parameters: {:?}",
                        GENERIC_ALIAS, id, params
                    ),
                ))
            } else if let Some(pos) = aliases
                .iter()
                .position(|(ident, _)| ident.to_string() == id)
            {
                write!(out, "{}", DisplayToTokens(&aliases[pos].1)).expect(WRITE_TO_MEM_FAILED_MSG);
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
