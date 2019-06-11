use petgraph::Direction;
use proc_macro2::{Span, TokenStream};
use smol_str::SmolStr;
use syn::{
    braced, bracketed, parenthesized, parse_quote, spanned::Spanned, token, Ident, LitStr, Token,
    Type,
};

use crate::{
    source_registry::SourceId,
    typemap::{
        ast::{is_second_subst_of_first, DisplayToTokens, SpannedSmolStr, TyParamsSubstMap},
        ty::FTypeConvCode,
        FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE, TO_VAR_TYPE_TEMPLATE,
    },
    FOREIGNER_CODE, FOREIGN_CODE,
};

static GENERIC_ALIAS: &str = "generic_alias";

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
        static DEFINE_C_TYPE: &str = "define_c_type";
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
                    match syn::parse2::<CTypes>(mac.tts.clone()) {
                        Ok(x) => {
                            if c_types.is_some() {
                                return Err(input
                                    .error(format!("{} should be used only once", DEFINE_C_TYPE)));
                            }
                            c_types = Some(x);
                        }
                        Err(_) => {
                            if generic_c_types.is_some() {
                                return Err(input.error(format!(
                                    "generic {} should be used only once",
                                    DEFINE_C_TYPE
                                )));
                            }
                            generic_c_types = Some(syn::parse2::<GenericCTypes>(mac.tts)?);
                        }
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
        Ok(CTypes { header_name, types })
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
    fn test_foreign_typemap_cpp_pair() {
        let rule = macro_to_conv_rule(parse_quote! {
            foreign_typemap!(
                generic_alias!(CRustPair = swig_concat_idents!(CRustPair, swig_i_type!(T1), swig_i_type!(T2)));
                define_c_type!(
                    module = "rust_tuple.h";
                    #[repr(C)]
                    pub struct CRustPair! {
                        first: swig_i_type!(T1),
                        second: swig_i_type!(T2),
                    }
                );
            ($p:r_type) <T1, T2> (T1, T2) => CRustPair!() {
                $out = CRustPair!() {
                    first: swig_from_rust_to_i_type!(T1, $p.0),
                    second: swig_from_rust_to_i_type!(T2, $p.1),
                }
            };
            ($p:r_type) <T1, T2> (T1, T2) <= CRustPair!() {
                $out = (swig_from_rust_i_type_to!(T1, $p.first), swig_from_rust_i_type_to!(T2, $p.second))
            };
            ($p:f_type, req_modules = ["\"rust_tuple.h\"", "<utility>"]) => "std::pair<swig_f_type!(T1), swig_f_type!(T2)>"
                    "std::make_pair(swig_foreign_from_i_type!(T1, $p.first), swig_foreign_from_i_type!(T2, $p.second))";
            ($p:f_type, req_modules = ["\"rust_tuple.h\"", "<utility>"]) <= "std::pair<swig_f_type!(T1), swig_f_type!(T2)>"
                "CRustPair! { swig_foreign_to_i_type!(T1, $p.first), swig_foreign_to_i_type!(T2, $p.second) }";
        )
        });
        println!("rule!!!: {:?}", rule);
        assert!(!rule.if_simple_rtype_ftype_map().is_some());
        assert!(rule.contains_data_for_language_backend());

        assert!(rule
            .is_ty_subst_of_my_generic_rtype(&parse_type! {i32}, Direction::Outgoing)
            .is_none());
        assert!(rule
            .is_ty_subst_of_my_generic_rtype(&parse_type! {()}, Direction::Outgoing)
            .is_none());
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

        let generics: syn::Generics = parse_quote! { <T1, T2> };
        assert_eq!(Some(generics), rule.rtype_generics);
        assert_eq!(
            RTypeConvRule {
                left_ty: parse_type! {(T1, T2)},
                right_ty: Some(parse_type! { CRustPair!() }),
                code: Some(FTypeConvCode::new(
                    "let {to_var}: {to_var_type} = CRustPair ! ( ) { first : swig_from_rust_to_i_type ! ( T1 , {from_var} . 0 ) , second : swig_from_rust_to_i_type ! ( T2 , {from_var} . 1 ) , };",
                    Span::call_site()
                )),
            },
            rule.rtype_left_to_right.unwrap()
        );

        assert_eq!(
            RTypeConvRule {
                left_ty: parse_type! { (T1, T2) },
                right_ty: Some(parse_type! { CRustPair!() }),
                code: Some(FTypeConvCode::new(
                    "let {to_var}: {to_var_type} = ( swig_from_rust_i_type_to ! ( T1 , {from_var} . first ) , swig_from_rust_i_type_to ! ( T2 , {from_var} . second ) );",
                    Span::call_site()
                )),
            },
            rule.rtype_right_to_left.unwrap()
        );
    }

    fn macro_to_conv_rule(mac: syn::Macro) -> TypeMapConvRuleInfo {
        let _ = env_logger::try_init();
        let code = mac.tts.to_string();
        syn::parse_str::<TypeMapConvRuleInfo>(&code)
            .unwrap_or_else(|err| panic_on_syn_error("macro_to_conv_rule", code, err))
    }
}
