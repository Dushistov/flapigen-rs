use petgraph::Direction;
use proc_macro2::{Span, TokenStream};
use smol_str::SmolStr;
use std::fmt::Write;
use syn::{
    braced, bracketed, parenthesized, parse_quote, spanned::Spanned, token, Ident, LitStr, Token,
    Type,
};

use crate::{
    error::{invalid_src_id_span, DiagnosticError, Result},
    source_registry::SourceId,
    typemap::{
        ast::{
            get_trait_bounds, is_second_subst_of_first, parse_ty_with_given_span,
            replace_all_types_with, DisplayToTokens, SpannedSmolStr, TyParamsSubstMap,
        },
        ty::FTypeConvCode,
        FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE, TO_VAR_TYPE_TEMPLATE,
    },
    FOREIGNER_CODE, FOREIGN_CODE,
};

static GENERIC_ALIAS: &str = "generic_alias";
static SWIG_CONCAT_IDENTS: &str = "swig_concat_idents";
static SWIG_I_TYPE: &str = "swig_i_type";
static DEFINE_C_TYPE: &str = "define_c_type";
static SWIG_FROM_RUST_TO_I_TYPE: &str = "swig_from_rust_to_i_type";
static SWIG_FROM_I_TYPE_TO_RUST: &str = "swig_from_i_type_to_rust";
static SWIG_FOREIGN_TO_I_TYPE: &str = "swig_foreign_to_i_type";
static SWIG_FOREIGN_FROM_I_TYPE: &str = "swig_foreign_from_i_type";
static SWIG_F_TYPE: &str = "swig_f_type";

#[derive(Debug)]
pub(crate) struct TypeMapConvRuleInfo {
    pub src_id: SourceId,
    pub rtype_generics: Option<syn::Generics>,
    pub rtype_left_to_right: Option<RTypeConvRule>,
    pub rtype_right_to_left: Option<RTypeConvRule>,
    pub ftype_left_to_right: Vec<FTypeConvRule>,
    pub ftype_right_to_left: Vec<FTypeConvRule>,
    /// For C++ case it is possible to introduce some C types
    pub c_types: Option<CTypes>,
    pub generic_c_types: Option<GenericCTypes>,
    pub f_code: Vec<ForeignCode>,
    pub generic_aliases: Vec<GenericAlias>,
}

impl TypeMapConvRuleInfo {
    pub(in crate::typemap) fn if_simple_rtype_ftype_map(
        &self,
    ) -> Option<(&Type, &FTypeName, &[SmolStr])> {
        if self.rtype_right_to_left.is_some() || !self.ftype_right_to_left.is_empty() {
            return None;
        }
        if self.ftype_left_to_right.len() > 1 {
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

    pub(crate) fn is_ty_subst_of_my_generic_rtype(
        &self,
        ty: &Type,
        direction: Direction,
    ) -> Option<TyParamsSubstMap> {
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

        Some(subst_map)
    }

    pub(crate) fn subst_generic_params_to_c_types(
        &self,
        param_map: &TyParamsSubstMap,
        expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
    ) -> Result<Option<CTypes>> {
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
                        let ty = param_map.get_by_str(param).unwrap_or(None).ok_or_else(|| {
                            DiagnosticError::new(
                                self.src_id,
                                code_span,
                                format!("unknown type parameter '{}'", param),
                            )
                        })?;

                        let i_type = expander.swig_i_type(&ty)?;
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
            let ctypes_list: CTypesList =
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
            c_types = Some(CTypes {
                header_name: generic_c_types.header_name.clone(),
                types: ctypes_list.0,
            });
        }
        Ok(c_types)
    }

    pub(crate) fn subst_generic_params(
        &self,
        param_map: TyParamsSubstMap,
        expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
    ) -> Result<Self> {
        assert!(self.is_generic());
        let type_aliases =
            build_generic_aliases(self.src_id, &self.generic_aliases, &param_map, expander)?;

        let rtype_left_to_right = expand_rtype_rule(
            self.src_id,
            self.rtype_left_to_right.as_ref(),
            &param_map,
            expander,
            &type_aliases,
        )?;
        let rtype_right_to_left = expand_rtype_rule(
            self.src_id,
            self.rtype_right_to_left.as_ref(),
            &param_map,
            expander,
            &type_aliases,
        )?;
        let ftype_left_to_right = expand_ftype_rule(
            self.src_id,
            self.ftype_left_to_right.as_ref(),
            &param_map,
            expander,
            &type_aliases,
        )?;
        let ftype_right_to_left = expand_ftype_rule(
            self.src_id,
            self.ftype_right_to_left.as_ref(),
            &param_map,
            expander,
            &type_aliases,
        )?;

        Ok(TypeMapConvRuleInfo {
            src_id: self.src_id,
            rtype_generics: None,
            rtype_left_to_right,
            rtype_right_to_left,
            ftype_left_to_right,
            ftype_right_to_left,
            //TODO: need macros expand?
            f_code: vec![],
            c_types: None,
            generic_c_types: None,
            generic_aliases: vec![],
        })
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
    pub req_modules: Vec<SmolStr>,
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

mod kw {
    use syn::custom_keyword;

    custom_keyword!(r_type);
    custom_keyword!(f_type);
    custom_keyword!(req_modules);
    custom_keyword!(module);
    custom_keyword!(option);
}

enum RuleType {
    RType(kw::r_type),
    FType(kw::f_type),
}

#[derive(Debug)]
enum ConvertRuleType<T> {
    LeftToRight(T),
    RightToLeft(T),
}

impl syn::parse::Parse for TypeMapConvRuleInfo {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut rtype_left_to_right: Option<RTypeConvRule> = None;
        let mut rtype_right_to_left: Option<RTypeConvRule> = None;
        let mut ftype_left_to_right = Vec::<FTypeConvRule>::new();
        let mut ftype_right_to_left = Vec::<FTypeConvRule>::new();
        let mut c_types = None;
        let mut f_code = Vec::<ForeignCode>::new();
        let mut generic_aliases = vec![];
        let mut generic_c_types = None;
        let mut rtype_generics = None;

        while !input.is_empty() {
            if input.peek(token::Paren) {
                let params;
                parenthesized!(params in input);

                let mut var_name = None;
                if params.peek(token::Dollar) {
                    params.parse::<token::Dollar>()?;
                    var_name = Some(params.parse::<Ident>()?);
                    params.parse::<Token![:]>()?;
                }

                let kw_la = params.lookahead1();
                let rule = if kw_la.peek(kw::r_type) {
                    RuleType::RType(params.parse::<kw::r_type>()?)
                } else if kw_la.peek(kw::f_type) {
                    RuleType::FType(params.parse::<kw::f_type>()?)
                } else {
                    return Err(kw_la.error());
                };
                let mut ftype_cfg: Option<SpannedSmolStr> = None;
                let mut ftype_req_modules = Vec::<SmolStr>::new();
                while !params.is_empty() && params.peek(Token![,]) {
                    params.parse::<Token![,]>()?;
                    let la = params.lookahead1();
                    if la.peek(kw::req_modules) {
                        if let RuleType::RType(ref keyword) = rule {
                            return Err(syn::Error::new(
                                keyword.span(),
                                format!(
                                    "{} may be used only with f_type",
                                    DisplayToTokens(&kw::req_modules::default())
                                ),
                            ));
                        }
                        params.parse::<kw::req_modules>()?;
                        params.parse::<Token![=]>()?;
                        let modules;
                        bracketed!(modules in params);
                        while !modules.is_empty() {
                            ftype_req_modules.push(modules.parse::<LitStr>()?.value().into());
                            if modules.peek(Token![,]) {
                                modules.parse::<Token![,]>()?;
                            } else if !modules.is_empty() {
                                return Err(modules.error(format!(
                                    "expect end of {} here",
                                    DisplayToTokens(&kw::req_modules::default())
                                )));
                            }
                        }
                    } else if la.peek(kw::option) {
                        if let RuleType::RType(ref rule) = rule {
                            return Err(syn::Error::new(
                                rule.span(),
                                "option allowed only for f_type",
                            ));
                        }
                        params.parse::<kw::option>()?;
                        params.parse::<Token![=]>()?;
                        let lit_str = params.parse::<LitStr>()?;
                        ftype_cfg = Some(SpannedSmolStr {
                            sp: lit_str.span(),
                            value: lit_str.value().into(),
                        });
                    } else {
                        return Err(la.error());
                    }
                }
                match rule {
                    RuleType::RType(keyword) => {
                        parse_r_type_rule(
                            input,
                            var_name,
                            keyword,
                            &mut rtype_left_to_right,
                            &mut rtype_right_to_left,
                            &mut rtype_generics,
                        )?;
                    }
                    RuleType::FType(keyword) => {
                        let left_ty = if input.peek(LitStr) {
                            Some(input.parse::<LitStr>()?.into())
                        } else {
                            None
                        };
                        let mut conv_rule_type = None;
                        if input.peek(Token![=>]) {
                            input.parse::<Token![=>]>()?;
                            conv_rule_type = Some(ConvertRuleType::LeftToRight(
                                input.parse::<LitStr>()?.into(),
                            ));
                        } else if input.peek(Token![<=]) {
                            input.parse::<Token![<=]>()?;
                            conv_rule_type = Some(ConvertRuleType::RightToLeft(
                                input.parse::<LitStr>()?.into(),
                            ));
                        }
                        let code = if conv_rule_type.is_some() && input.peek(LitStr) {
                            let code_str = input.parse::<LitStr>()?;
                            let var_name = var_name.ok_or_else(|| {
                                syn::Error::new(keyword.span(), "there is conversation code, but name of input variable not defined here")
                            })?;
                            let var_name = format!("${}", var_name);
                            Some(FTypeConvCode::new(
                                code_str.value().replace(&var_name, FROM_VAR_TEMPLATE),
                                code_str.span(),
                            ))
                        } else {
                            None
                        };
                        match conv_rule_type {
                            Some(ConvertRuleType::LeftToRight(right_ty)) => {
                                if ftype_left_to_right
                                    .iter()
                                    .any(|x| x.cfg_option == ftype_cfg)
                                {
                                    return Err(syn::Error::new(
                                        keyword.span(),
                                        "duplicate of f_type left to right rule with the same option",
                                    ));
                                }
                                ftype_left_to_right.push(FTypeConvRule {
                                    req_modules: ftype_req_modules,
                                    cfg_option: ftype_cfg,
                                    left_right_ty: if let Some(left_ty) = left_ty {
                                        FTypeLeftRightPair::Both(left_ty, right_ty)
                                    } else {
                                        FTypeLeftRightPair::OnlyRight(right_ty)
                                    },
                                    code,
                                });
                            }
                            Some(ConvertRuleType::RightToLeft(right_ty)) => {
                                if ftype_right_to_left
                                    .iter()
                                    .any(|x| x.cfg_option == ftype_cfg)
                                {
                                    return Err(syn::Error::new(
                                        keyword.span(),
                                        "duplicate of f_type right to left rule with the same option",
                                    ));
                                }
                                ftype_right_to_left.push(FTypeConvRule {
                                    cfg_option: ftype_cfg,
                                    req_modules: ftype_req_modules,
                                    left_right_ty: if let Some(left_ty) = left_ty {
                                        FTypeLeftRightPair::Both(left_ty, right_ty)
                                    } else {
                                        FTypeLeftRightPair::OnlyRight(right_ty)
                                    },
                                    code,
                                });
                            }
                            None => {
                                if ftype_left_to_right
                                    .iter()
                                    .any(|x| x.cfg_option == ftype_cfg)
                                {
                                    return Err(syn::Error::new(
                                        keyword.span(),
                                        "duplicate of f_type left to right rule with the same option",
                                    ));
                                }
                                let left_ty = left_ty.ok_or_else(|| {
                                    syn::Error::new(
                                        keyword.span(),
                                        "expect type name in this kind of f_type rule",
                                    )
                                })?;
                                ftype_left_to_right.push(FTypeConvRule {
                                    req_modules: ftype_req_modules,
                                    cfg_option: ftype_cfg,
                                    left_right_ty: FTypeLeftRightPair::OnlyLeft(left_ty),
                                    code: None,
                                });
                            }
                        }
                    }
                }
            } else {
                let mac: syn::Macro = input.parse()?;
                let is_our_macro = [DEFINE_C_TYPE, FOREIGNER_CODE, FOREIGN_CODE, GENERIC_ALIAS]
                    .iter()
                    .any(|x| mac.path.is_ident(x));
                if !is_our_macro {
                    return Err(syn::Error::new(mac.span(), "unknown macro in this context"));
                }
                if mac.path.is_ident(DEFINE_C_TYPE) {
                    if c_types.is_some() || generic_c_types.is_some() {
                        return Err(
                            input.error(format!("{} should be used only once", DEFINE_C_TYPE))
                        );
                    }
                    match syn::parse2::<CTypes>(mac.tts.clone()) {
                        Ok(x) => c_types = Some(x),

                        Err(_) => generic_c_types = Some(syn::parse2::<GenericCTypes>(mac.tts)?),
                    }
                } else if mac.path.is_ident(FOREIGN_CODE) || mac.path.is_ident(FOREIGNER_CODE) {
                    let fc_elem = syn::parse2::<ForeignCode>(mac.tts)?;
                    f_code.push(fc_elem);
                } else if mac.path.is_ident(GENERIC_ALIAS) {
                    generic_aliases.push(syn::parse2::<GenericAlias>(mac.tts)?);
                } else {
                    unreachable!();
                }
            }
            input.parse::<Token![;]>()?;
        }

        let rule = TypeMapConvRuleInfo {
            src_id: SourceId::none(),
            rtype_generics,
            c_types,
            f_code,
            rtype_left_to_right,
            rtype_right_to_left,
            ftype_left_to_right,
            ftype_right_to_left,
            generic_aliases,
            generic_c_types,
        };

        if !rule.generic_aliases.is_empty() && !rule.is_generic() {
            Err(syn::Error::new(
                rule.generic_aliases[0].alias.span(),
                format!("there is {}, but r_type is not generic", GENERIC_ALIAS),
            ))
        } else if rule.generic_c_types.is_some() && !rule.is_generic() {
            Err(syn::Error::new(
                rule.generic_aliases[0].alias.span(),
                format!(
                    "there is generic {}, but r_type is not generic",
                    DEFINE_C_TYPE
                ),
            ))
        } else {
            Ok(rule)
        }
    }
}

fn parse_r_type_rule(
    input: syn::parse::ParseStream,
    var_name: Option<syn::Ident>,
    keyword: kw::r_type,
    rtype_left_to_right: &mut Option<RTypeConvRule>,
    rtype_right_to_left: &mut Option<RTypeConvRule>,
    generics: &mut Option<syn::Generics>,
) -> syn::Result<()> {
    if input.peek(Token![<]) {
        let new_generics = input.parse::<syn::Generics>()?;
        if let Some(prev_generics) = generics.as_ref() {
            if *prev_generics != new_generics {
                return Err(syn::Error::new(
                    new_generics.span(),
                    "generic descriptions are different for r_type <= and r_type =>",
                ));
            }
        } else {
            if !get_trait_bounds(&new_generics).is_empty() {
                unimplemented!();
            }
            *generics = Some(new_generics);
        }
    }
    let left_ty = input.parse::<Type>()?;
    let mut conv_rule_type = None;
    if input.peek(Token![=>]) {
        input.parse::<Token![=>]>()?;
        conv_rule_type = Some(ConvertRuleType::LeftToRight(input.parse::<Type>()?));
    } else if input.peek(Token![<=]) {
        input.parse::<Token![<=]>()?;
        conv_rule_type = Some(ConvertRuleType::RightToLeft(input.parse::<Type>()?));
    }

    let code = if conv_rule_type.is_some() && input.peek(syn::token::Brace) {
        let content;
        braced!(content in input);
        let conv_body = content.parse::<TokenStream>()?;

        let var_name = var_name.ok_or_else(|| {
            syn::Error::new(
                keyword.span(),
                "there is conversation code, but name of input variable not defined here",
            )
        })?;
        //because of $var most likely will be reformated to "$ var", so
        //without clue how syn formatted token -> string, just convert text to tokens and back
        let d_var_name: TokenStream = parse_quote!($#var_name);
        let d_var_name = d_var_name.to_string();
        let out_var: TokenStream = parse_quote!($out);
        let out_var = out_var.to_string();
        let mut code_str = conv_body.to_string();
        if !code_str.contains(&d_var_name) || !code_str.contains(&out_var) {
            return Err(syn::Error::new(
                conv_body.span(),
                format!("no $out or ${} in conversation code", var_name),
            ));
        }
        code_str.push(';');

        Some(FTypeConvCode::new2(
            code_str.replace(&d_var_name, FROM_VAR_TEMPLATE).replace(
                &out_var,
                &format!("let {}: {}", TO_VAR_TEMPLATE, TO_VAR_TYPE_TEMPLATE),
            ),
            conv_body.span(),
        ))
    } else {
        None
    };

    match conv_rule_type {
        Some(ConvertRuleType::LeftToRight(right_ty)) => {
            if rtype_left_to_right.is_some() {
                return Err(syn::Error::new(
                    keyword.span(),
                    "duplicate of r_type left to right rule",
                ));
            }
            *rtype_left_to_right = Some(RTypeConvRule {
                left_ty,
                right_ty: Some(right_ty),
                code,
            });
        }
        Some(ConvertRuleType::RightToLeft(right_ty)) => {
            if rtype_right_to_left.is_some() {
                return Err(syn::Error::new(
                    keyword.span(),
                    "duplicate of r_type right to left rule",
                ));
            }
            *rtype_right_to_left = Some(RTypeConvRule {
                left_ty,
                right_ty: Some(right_ty),
                code,
            });
        }
        None => {
            if rtype_left_to_right.is_some() {
                return Err(syn::Error::new(
                    keyword.span(),
                    "duplicate of r_type left to right rule",
                ));
            }
            *rtype_left_to_right = Some(RTypeConvRule {
                left_ty,
                right_ty: None,
                code: None,
            });
        }
    }
    Ok(())
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum CType {
    Struct(syn::ItemStruct),
    Union(syn::ItemUnion),
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct CTypes {
    pub header_name: SmolStr,
    pub types: Vec<CType>,
}

impl syn::parse::Parse for CTypes {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<kw::module>()?;
        input.parse::<Token![=]>()?;
        let module_name: LitStr = input.parse()?;
        let header_name: SmolStr = module_name.value().into();
        input.parse::<Token![;]>()?;
        let ctypes_list: CTypesList = input.parse()?;
        Ok(CTypes {
            header_name,
            types: ctypes_list.0,
        })
    }
}

struct CTypesList(Vec<CType>);

impl syn::parse::Parse for CTypesList {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut types = vec![];
        while !input.is_empty() {
            let item: syn::Item = input.parse()?;
            match item {
                syn::Item::Struct(s) => {
                    if !has_repr_c_attr(&s.attrs) {
                        return Err(syn::Error::new(s.span(), "struct has no repr(C) attribute"));
                    }
                    types.push(CType::Struct(s));
                }
                syn::Item::Union(u) => {
                    if !has_repr_c_attr(&u.attrs) {
                        return Err(syn::Error::new(u.span(), "union has no repr(C) attribute"));
                    }
                    types.push(CType::Union(u));
                }
                _ => return Err(syn::Error::new(item.span(), "Expect struct or union here")),
            }
        }
        Ok(CTypesList(types))
    }
}

fn has_repr_c_attr(attrs: &[syn::Attribute]) -> bool {
    let repr_c_attr: syn::Attribute = parse_quote! { #[repr(C)] };
    attrs.iter().any(|a| *a == repr_c_attr)
}

#[derive(Debug)]
pub(crate) struct ForeignCode {
    pub module_name: SmolStr,
    pub cfg_option: Option<SpannedSmolStr>,
    pub code: String,
}

impl syn::parse::Parse for ForeignCode {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<kw::module>()?;
        input.parse::<Token![=]>()?;
        let module_name: LitStr = input.parse()?;
        let module_name: SmolStr = module_name.value().into();
        input.parse::<Token![;]>()?;
        let cfg_option: Option<SpannedSmolStr> = if input.peek(kw::option) {
            input.parse::<kw::option>()?;
            input.parse::<Token![=]>()?;
            let cfg_option: LitStr = input.parse()?;
            input.parse::<Token![;]>()?;
            Some(SpannedSmolStr {
                sp: cfg_option.span(),
                value: cfg_option.value().into(),
            })
        } else {
            None
        };
        let code: LitStr = input.parse()?;

        Ok(ForeignCode {
            module_name,
            cfg_option,
            code: code.value(),
        })
    }
}

#[derive(Debug)]
pub(crate) struct GenericAlias {
    pub alias: syn::Ident,
    pub value: TokenStream,
}

impl syn::parse::Parse for GenericAlias {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let alias = input.parse()?;
        input.parse::<Token![=]>()?;
        let value = input.parse()?;
        Ok(GenericAlias { alias, value })
    }
}

#[derive(Debug)]
pub(crate) struct GenericCTypes {
    pub header_name: SmolStr,
    pub types: TokenStream,
}

impl syn::parse::Parse for GenericCTypes {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<kw::module>()?;
        input.parse::<Token![=]>()?;
        let module_name: LitStr = input.parse()?;
        let header_name: SmolStr = module_name.value().into();
        input.parse::<Token![;]>()?;
        let types = input.parse()?;
        Ok(GenericCTypes { header_name, types })
    }
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
    fn swig_f_type(&mut self, ty: &syn::Type) -> Result<SmolStr>;
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
            let ty: &syn::Type = param_map.get(&id).unwrap_or(None).ok_or_else(|| {
                DiagnosticError::new(
                    src_id,
                    id.span(),
                    format!("unknown type parameter '{}'", id),
                )
            })?;
            let i_type: syn::Type = expander.swig_i_type(ty)?;
            ident.push_str(&DisplayToTokens(&i_type).to_string());
        }
        GenericAliasItem::Ident(id) => ident.push_str(&id.to_string()),
    }
    Ok(())
}

impl syn::parse::Parse for GenericAliasItem {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.fork().parse::<syn::Macro>().is_ok() {
            let mac: syn::Macro = input.parse()?;
            if mac.path.is_ident(SWIG_CONCAT_IDENTS) {
                let items: GenericAliasItemVecCommaSeparated = syn::parse2(mac.tts)?;
                Ok(GenericAliasItem::Concat(items.0))
            } else if mac.path.is_ident(SWIG_I_TYPE) {
                let item: syn::Ident = syn::parse2(mac.tts)?;
                Ok(GenericAliasItem::SwigIType(item))
            } else {
                return Err(syn::Error::new(
                    mac.span(),
                    format!(
                        "uknown macro '{}' in this context",
                        DisplayToTokens(&mac.path)
                    ),
                ));
            }
        } else {
            Ok(GenericAliasItem::Ident(input.parse()?))
        }
    }
}

struct GenericAliasItemVecCommaSeparated(Vec<GenericAliasItem>);

impl syn::parse::Parse for GenericAliasItemVecCommaSeparated {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let ret =
            syn::punctuated::Punctuated::<GenericAliasItem, Token![,]>::parse_terminated(input)?;
        Ok(GenericAliasItemVecCommaSeparated(ret.into_iter().collect()))
    }
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
    next_pos = next_pos + (&code[next_pos..]).find(')')?;
    let cnt_end = next_pos;
    let id = &code[0..id_end];
    let params: Vec<&str> = (&code[cnt_start..cnt_end])
        .trim()
        .split(',')
        .map(|x| x.trim())
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

                            let ty = param_map.get_by_str(type_name).unwrap_or(None).ok_or_else(
                                || {
                                    DiagnosticError::new(
                                        src_id,
                                        x.span(),
                                        format!("unknown type parameter '{}'", type_name),
                                    )
                                },
                            )?;
                            let tt: String = if id == SWIG_FROM_RUST_TO_I_TYPE {
                                expander.swig_from_rust_to_i_type(&ty, in_var_name, out_var_name)?
                            } else if id == SWIG_FROM_I_TYPE_TO_RUST {
                                expander.swig_from_i_type_to_rust(&ty, in_var_name, out_var_name)?
                            } else {
                                unreachable!()
                            };
                            write!(out, "{}", tt).expect("write to String failed");
                        }
                        _ => {
                            let alias_idx = generic_aliases
                                .iter()
                                .position(|a| a.0 == id)
                                .ok_or_else(|| {
                                    DiagnosticError::new(
                                        src_id,
                                        x.span(),
                                        format!("unknown {} {}", GENERIC_ALIAS, id),
                                    )
                                })?;
                            write!(out, "{}", DisplayToTokens(&generic_aliases[alias_idx].1))
                                .expect("write to String failed");
                        }
                    }
                    Ok(())
                },
            )?;
            Some(FTypeConvCode::new(code, x.span()))
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
        let left_right_ty = match grule.left_right_ty {
            OnlyLeft(ref ftype) => OnlyLeft(expand_ftype_name(src_id, ftype, param_map, expander)?),
            OnlyRight(ref ftype) => {
                OnlyRight(expand_ftype_name(src_id, ftype, param_map, expander)?)
            }
            Both(ref fl, ref fr) => Both(
                expand_ftype_name(src_id, fl, param_map, expander)?,
                expand_ftype_name(src_id, fr, param_map, expander)?,
            ),
        };
        let code = match grule.code {
            Some(ref x) => {
                let code = expand_macroses(
                    x.as_str(),
                    |id: &str, params: Vec<&str>, out: &mut String| -> Result<()> {
                        match id {
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

                                let ty = param_map
                                    .get_by_str(type_name)
                                    .unwrap_or(None)
                                    .ok_or_else(|| {
                                        DiagnosticError::new(
                                            src_id,
                                            x.span(),
                                            format!("unknown type parameter '{}'", type_name),
                                        )
                                    })?;
                                let tt: String = if id == SWIG_FOREIGN_TO_I_TYPE {
                                    expander.swig_foreign_to_i_type(&ty, var_name)?
                                } else if id == SWIG_FOREIGN_FROM_I_TYPE {
                                    expander.swig_foreign_from_i_type(&ty, var_name)?
                                } else {
                                    unreachable!()
                                };
                                write!(out, "{}", tt).expect("write to String failed");
                            }
                            _ => {
                                let alias_idx = generic_aliases
                                    .iter()
                                    .position(|a| a.0 == id)
                                    .ok_or_else(|| {
                                        DiagnosticError::new(
                                            src_id,
                                            x.span(),
                                            format!("unknown {} {}", GENERIC_ALIAS, id),
                                        )
                                    })?;

                                write!(
                                    out,
                                    "{}",
                                    expander
                                        .swig_f_type(&generic_aliases[alias_idx].1)?
                                        .replace("struct", "")
                                        .replace("union", "")
                                )
                                .expect("write to String failed");
                            }
                        }
                        Ok(())
                    },
                )?;
                Some(FTypeConvCode::new(code, x.span()))
            }
            None => None,
        };
        ret.push(FTypeConvRule {
            req_modules: grule.req_modules.clone(),
            cfg_option: grule.cfg_option.clone(),
            left_right_ty,
            code,
        });
    }
    Ok(ret)
}

fn expand_ftype_name(
    src_id: SourceId,
    ftype: &FTypeName,
    param_map: &TyParamsSubstMap,
    expander: &mut dyn TypeMapConvRuleInfoExpanderHelper,
) -> Result<FTypeName> {
    let new_fytpe = expand_macroses(
        ftype.name.as_str(),
        |id: &str, params: Vec<&str>, out: &mut String| {
            if id == SWIG_F_TYPE {
                let type_name = if params.len() == 1 {
                    &params[0]
                } else {
                    return Err(DiagnosticError::new(
                        src_id,
                        ftype.sp,
                        format!(
                            "{} parameters in {} instead of 1",
                            params.len(),
                            SWIG_F_TYPE
                        ),
                    ));
                };

                let ty = param_map
                    .get_by_str(type_name)
                    .unwrap_or(None)
                    .ok_or_else(|| {
                        DiagnosticError::new(
                            src_id,
                            ftype.sp,
                            format!("unknown type parameter '{}'", type_name),
                        )
                    })?;
                out.push_str(&expander.swig_f_type(ty)?);
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::panic_on_syn_error;
    use syn::parse_quote;

    #[test]
    fn test_foreign_typemap_qdatetime() {
        let rule = macro_to_conv_rule(parse_quote! {
            foreign_typemap!(
                ($pin:r_type) DateTime<Utc> => i64 {
                    $out = $pin.timestamp()
                };
                ($pin:f_type) => "QDateTime" r#"
$out = QDateTime::fromMSecsSinceEpoch($pin * 1000, Qt::UTC, 0);
        "#;
            )
        });
        assert!(!rule.if_simple_rtype_ftype_map().is_some());
        assert!(!rule.contains_data_for_language_backend());
    }

    #[test]
    fn test_foreign_typemap_cpp_bool() {
        let rule = macro_to_conv_rule(parse_quote! {
            foreign_typemap!(
                ($pin:r_type) bool => ::std::os::raw::c_char {
                    $out = if $pin  { 1 } else { 0 }
                };
                ($pin:f_type) => "bool" "$out = ($pin != 0);";
                ($pin:r_type) bool <= ::std::os::raw::c_char {
                    $out = ($pin != 0)
                };
                ($pin:f_type) <= "bool" "$out = $pin ? 1 : 0;";
            )
        });
        println!("rule {:?}", rule);
        assert!(!rule.is_generic());
        assert!(!rule.if_simple_rtype_ftype_map().is_some());
        assert!(!rule.contains_data_for_language_backend());

        assert_eq!(
            RTypeConvRule {
                left_ty: parse_type!(bool),
                right_ty: Some(parse_type!(::std::os::raw::c_char)),
                code: Some(FTypeConvCode::new(
                    "let {to_var}: {to_var_type} = if {from_var} { 1 } else { 0 };",
                    Span::call_site()
                )),
            },
            rule.rtype_left_to_right.unwrap()
        );

        assert_eq!(
            RTypeConvRule {
                left_ty: parse_type!(bool),
                right_ty: Some(parse_type!(::std::os::raw::c_char)),
                code: Some(FTypeConvCode::new(
                    "let {to_var}: {to_var_type} = ( {from_var} != 0 );",
                    Span::call_site()
                )),
            },
            rule.rtype_right_to_left.unwrap()
        );

        assert_eq!(
            vec![FTypeConvRule {
                req_modules: vec![],
                left_right_ty: FTypeLeftRightPair::OnlyRight(FTypeName {
                    name: "bool".into(),
                    sp: Span::call_site(),
                }),
                code: Some(FTypeConvCode::new(
                    "$out = ({from_var} != 0);",
                    Span::call_site()
                )),
                cfg_option: None,
            }],
            rule.ftype_left_to_right
        );

        assert_eq!(
            vec![FTypeConvRule {
                req_modules: vec![],
                left_right_ty: FTypeLeftRightPair::OnlyRight(FTypeName {
                    name: "bool".into(),
                    sp: Span::call_site(),
                }),
                code: Some(FTypeConvCode::new(
                    "$out = {from_var} ? 1 : 0;",
                    Span::call_site()
                )),
                cfg_option: None,
            }],
            rule.ftype_right_to_left
        );
    }

    #[test]
    fn test_foreign_typemap_qstring() {
        let rule = macro_to_conv_rule(parse_quote! {
            foreign_typemap!(
                ($rin:r_type) &str => RustStrView {
                    $out = RustStrView::from_str($rin)
                };
                ($pin:f_type) => "QString" r#"
$out = QString::fromUtf8($pin.data, $pin.len);
"#;)
        });
        assert!(!rule.if_simple_rtype_ftype_map().is_some());
        assert!(!rule.contains_data_for_language_backend());
    }

    #[ignore]
    #[test]
    fn test_foreign_typemap_callback_to_qfuture() {
        let rule = macro_to_conv_rule(parse_quote! {
            foreign_typemap!(
                define_c_type!(
                    #[repr(C)]
                    struct concat_ident!(CFnOnce, swig_i_type!(T)) {
                        cb: extern "C" fn(swig_i_type!(T), *mut c_void),
                        ctx: *mut c_void,
                    });

                ($pin:r_type) <T, F: FnOnce(T)> F <= concat_ident!(CFnOnce, swig_i_type!(T))
                {
                    $out = |x| $pin.cb(convert_to_c!(x), $pin.ctx)
                };

                define_helper_f_helper!(
                    void concat_ident!(result_ready, swig_i_type!(T))(swig_i_type!(T) ret, void *ctx)
                    {
                        swig_f_type!(T) cpp_ret = convert_to_f!(ret);
                        auto ptr = std::make_shared<swig_f_type!(T)>(std::move(cpp_ret));
                        auto fi =
                            static_cast<QFutureInterface<std::shared_ptr<swig_f_type!(T)>> *>(ctx);
                        fi->reportResult(std::move(ptr));
                        fi->reportFinished();
                        delete fi;
                    }
                );

                ($out:f_type, $pin:input_to_output) => "QFuture<std::shared_ptr<swig_f_type!(T)>>" r#"
        auto fi = new QFutureInterface<std::shared_ptr<swig_f_type!(T)>>;
        concat_ident!(CFnOnce, swig_i_type!(T)) cb;
        cb.cb = concat_ident!(result_ready, swig_i_type!(T));
        cb.ctx = fi;
        $out = fi->future();
        $pin = cb;
 }"#;
            )
        });
        assert!(!rule.if_simple_rtype_ftype_map().is_some());
        assert!(rule.contains_data_for_language_backend());
    }

    #[test]
    fn test_foreign_typemap_java_datetime() {
        let rule = macro_to_conv_rule(parse_quote! {
            foreign_typemap!(
                ($pin:r_type) SystemTime => jlong {
                    let since_unix_epoch = $pin.duration_since(::std::time::UNIX_EPOCH).unwrap();
                    $out = (since_unix_epoch.as_secs() * 1_000
                            + (since_unix_epoch.subsec_nanos() / 1_000_000) as u64)
                        as jlong;
                };
                ($pin:f_type) => "java.util.Date" "$out = new java.util.Date($pin);";
            )
        });
        assert!(!rule.if_simple_rtype_ftype_map().is_some());
        assert!(!rule.contains_data_for_language_backend());
    }

    #[ignore]
    #[test]
    fn test_foreign_typemap_jstring() {
        let rule = macro_to_conv_rule(parse_quote! {
            foreign_typemap!(
                ($pin:r_type, $jstr:variable) &str <= jstring {
                    $jstr = JavaString::new(env, $pin);
                    $out = $jstr.to_str();
                };
                ($pin:f_type, non_null) <= "String";
            )
        });
        assert!(!rule.if_simple_rtype_ftype_map().is_some());
        assert!(rule.contains_data_for_language_backend());
    }

    #[test]
    fn test_foreign_typemap_simple_typemap() {
        let rule = macro_to_conv_rule(parse_quote! {
            foreign_typemap!(
                (r_type) jlong;
                (f_type) "long";
            )
        });
        assert!(rule.if_simple_rtype_ftype_map().is_some());
        assert!(!rule.contains_data_for_language_backend());

        assert_eq!(
            RTypeConvRule {
                left_ty: parse_type!(jlong),
                right_ty: None,
                code: None,
            },
            rule.rtype_left_to_right.unwrap()
        );

        assert_eq!(None, rule.rtype_right_to_left);

        assert_eq!(
            vec![FTypeConvRule {
                req_modules: vec![],
                left_right_ty: FTypeLeftRightPair::OnlyLeft(FTypeName {
                    name: "long".into(),
                    sp: Span::call_site(),
                }),
                code: None,
                cfg_option: None,
            }],
            rule.ftype_left_to_right
        );

        assert_eq!(Vec::<FTypeConvRule>::new(), rule.ftype_right_to_left);
    }

    #[test]
    fn test_foreign_typemap_cpp_ruststring() {
        let rule = macro_to_conv_rule(parse_quote! {
            foreign_typemap!(
                define_c_type!(module = "rust_str.h";
                    #[repr(C)]
                    struct CRustString {
                        data: *const ::std::os::raw::c_char,
                        len: usize,
                        capacity: usize,
                    }
                );
                foreigner_code!(module = "rust_str.h";
                                option="boost";
                                r#"
        namespace $RUST_SWIG_USER_NAMESPACE {
        class RustString final : private CRustString {
        public:

        };
        }
        "#
                                );
                ($pin:r_type) String => CRustString {
                    $out = CRustString::from_string($pin)
                };
                ($pin:f_type, req_modules = ["rust_str.h"]) => "RustString" "RustString{$pin}";
            )
        });
        assert!(!rule.if_simple_rtype_ftype_map().is_some());
        assert!(rule.contains_data_for_language_backend());
        assert_eq!(
            CTypes {
                header_name: "rust_str.h".into(),
                types: vec![CType::Struct(parse_quote! {
                    #[repr(C)]
                        struct CRustString {
                            data: *const ::std::os::raw::c_char,
                            len: usize,
                            capacity: usize,
                        }
                }),],
            },
            rule.c_types.unwrap()
        );
    }

    #[test]
    fn test_foreign_typemap_cpp_str() {
        let rule = macro_to_conv_rule(parse_quote! {
                    foreign_typemap!(
            define_c_type!(module = "rust_str.h";
                           #[repr(C)]
                           pub struct CRustStrView {
                               data: *const ::std::os::raw::c_char,
                               len: usize,
                           }
            );
            ($p:r_type) &str => CRustStrView {
                $out = CRustStrView::from_str($p)
            };
            ($p:f_type, option = "CppStrView::Boost", req_modules = ["\"rust_str.h\"", "<boost/utility/string_view.hpp>"]) => "boost::string_view"
                "boost::string_view{ $p.data, $p.len }";
            ($p:f_type, option = "CppStrView::Std17", req_modules = ["\"rust_str.h\"", "<string_view>"]) => "std::string_view"
                "std::string_view{ $p.data, $p.len }";
        )
                });
        assert!(!rule.if_simple_rtype_ftype_map().is_some());
        assert!(rule.contains_data_for_language_backend());
    }

    #[test]
    fn test_foreign_typemap_cpp_pair_expand() {
        let rule = cpp_pair_rule();
        println!("rule!!!: {:?}", rule);
        assert!(rule.is_generic());
        struct Dummy;
        impl TypeMapConvRuleInfoExpanderHelper for Dummy {
            fn swig_i_type(&mut self, ty: &syn::Type) -> Result<syn::Type> {
                Ok(ty.clone())
            }
            fn swig_from_rust_to_i_type(
                &mut self,
                _ty: &syn::Type,
                in_var_name: &str,
                out_var_name: &str,
            ) -> Result<String> {
                Ok(format!("{} = {}", out_var_name, in_var_name))
            }
            fn swig_from_i_type_to_rust(
                &mut self,
                ty: &syn::Type,
                in_var_name: &str,
                out_var_name: &str,
            ) -> Result<String> {
                self.swig_from_rust_to_i_type(ty, in_var_name, out_var_name)
            }
            fn swig_f_type(&mut self, ty: &syn::Type) -> Result<SmolStr> {
                if *ty == parse_type!(i32) {
                    Ok("int32_t".into())
                } else if *ty == parse_type!(f32) {
                    Ok("float".into())
                } else if *ty == parse_type!(CRustPairi32f32) {
                    Ok("CRustPairi32f32".into())
                } else {
                    panic!("swig_f_type: Unknown type: {}", DisplayToTokens(ty));
                }
            }
            fn swig_foreign_to_i_type(
                &mut self,
                _ty: &syn::Type,
                var_name: &str,
            ) -> Result<String> {
                Ok(var_name.into())
            }
            fn swig_foreign_from_i_type(
                &mut self,
                ty: &syn::Type,
                var_name: &str,
            ) -> Result<String> {
                self.swig_foreign_to_i_type(ty, var_name)
            }
        }

        let subst_params = rule
            .is_ty_subst_of_my_generic_rtype(&parse_type! {(i32, f32)}, Direction::Outgoing)
            .unwrap();
        let c_types = rule
            .subst_generic_params_to_c_types(&subst_params, &mut Dummy)
            .unwrap()
            .unwrap();

        assert_eq!(
            c_types,
            CTypes {
                header_name: "rust_tuple.h".into(),
                types: vec![CType::Struct(parse_quote! {
                    #[repr(C)]
                    pub struct CRustPairi32f32 {
                        first: i32,
                        second: f32,
                    }
                }),],
            },
        );

        let new_rule = rule.subst_generic_params(subst_params, &mut Dummy).unwrap();
        assert!(!new_rule.is_generic());
        assert!(!new_rule.contains_data_for_language_backend());
    }

    #[test]
    fn test_foreign_typemap_cpp_pair_syntax() {
        let rule = cpp_pair_rule();
        println!("rule!!!: {:?}", rule);
        assert!(!rule.if_simple_rtype_ftype_map().is_some());
        assert!(rule.contains_data_for_language_backend());
        assert!(rule.is_generic());

        assert!(rule
            .is_ty_subst_of_my_generic_rtype(&parse_type! {i32}, Direction::Outgoing)
            .is_none());
        assert!(rule
            .is_ty_subst_of_my_generic_rtype(&parse_type! {()}, Direction::Outgoing)
            .is_none());

        let generics: syn::Generics = parse_quote! { <T1, T2> };
        assert_eq!(generics, *rule.rtype_generics.as_ref().unwrap());
        assert_eq!(
            RTypeConvRule {
                left_ty: parse_type! {(T1, T2)},
                right_ty: Some(parse_type! { CRustPair!() }),
                code: Some(FTypeConvCode::new(
                    concat!(
                        "swig_from_rust_to_i_type ! ( T1 , {from_var} . 0 , p0 ) ; ",
                        "swig_from_rust_to_i_type ! ( T2 , {from_var} . 1 , p1 ) ; ",
                        "let {to_var}: {to_var_type} = CRustPair ! ( ) { first : p0 , second : p1 , };"),
                    Span::call_site()
                )),
            },
            *rule.rtype_left_to_right.as_ref().unwrap()
        );

        assert_eq!(
            RTypeConvRule {
                left_ty: parse_type! { (T1, T2) },
                right_ty: Some(parse_type! { CRustPair!() }),
                code: Some(FTypeConvCode::new(
                    concat!(
                        "swig_from_i_type_to_rust ! ( T1 , {from_var} . first , p0 ) ; ",
                        "swig_from_i_type_to_rust ! ( T2 , {from_var} . second , p1 ) ; ",
                        "let {to_var}: {to_var_type} = ( p0 , p1 );"
                    ),
                    Span::call_site()
                )),
            },
            *rule.rtype_right_to_left.as_ref().unwrap()
        );

        let t1 = syn::Ident::new("T1", Span::call_site());
        let t2 = syn::Ident::new("T2", Span::call_site());
        assert_eq!(
            Some({
                let mut subst_map = TyParamsSubstMap::default();
                subst_map.insert(&t1, Some(parse_type! {i32}));
                subst_map.insert(&t2, Some(parse_type! {f32}));
                subst_map
            }),
            rule.is_ty_subst_of_my_generic_rtype(&parse_type! {(i32, f32)}, Direction::Outgoing)
        );
    }

    fn cpp_pair_rule() -> TypeMapConvRuleInfo {
        macro_to_conv_rule(parse_quote! {
            foreign_typemap!(
                generic_alias!(CRustPair = swig_concat_idents!(CRustPair, swig_i_type!(T1), swig_i_type!(T2)));
                define_c_type!(
                    module = "rust_tuple.h";
                    #[repr(C)]
                    pub struct CRustPair!() {
                        first: swig_i_type!(T1),
                        second: swig_i_type!(T2),
                    }
                );
                ($p:r_type) <T1, T2> (T1, T2) => CRustPair!() {
                    swig_from_rust_to_i_type!(T1, $p.0, p0);
                    swig_from_rust_to_i_type!(T2, $p.1, p1);
                    $out = CRustPair!() {
                        first: p0,
                        second: p1,
                    }
                };
                ($p:r_type) <T1, T2> (T1, T2) <= CRustPair!() {
                    swig_from_i_type_to_rust!(T1, $p.first, p0);
                    swig_from_i_type_to_rust!(T2, $p.second, p1);
                    $out = (p0, p1)
                };
                ($p:f_type, req_modules = ["\"rust_tuple.h\"", "<utility>"]) => "std::pair<swig_f_type!(T1), swig_f_type!(T2)>"
                    "std::make_pair(swig_foreign_from_i_type!(T1, $p.first), swig_foreign_from_i_type!(T2, $p.second))";
                ($p:f_type, req_modules = ["\"rust_tuple.h\"", "<utility>"]) <= "std::pair<swig_f_type!(T1), swig_f_type!(T2)>"
                    "CRustPair!() { swig_foreign_to_i_type!(T1, $p.first), swig_foreign_to_i_type!(T2, $p.second) }";
            )
        })
    }

    fn macro_to_conv_rule(mac: syn::Macro) -> TypeMapConvRuleInfo {
        let _ = env_logger::try_init();
        let code = mac.tts.to_string();
        syn::parse_str::<TypeMapConvRuleInfo>(&code)
            .unwrap_or_else(|err| panic_on_syn_error("macro_to_conv_rule", code, err))
    }
}
