use proc_macro2::TokenStream;
use smol_str::SmolStr;
use syn::{
    braced, bracketed, parenthesized, parse_quote, spanned::Spanned, token, Ident, LitStr, Token,
    Type,
};

use super::{
    CItem, CItems, FTypeConvRule, FTypeLeftRightPair, ForeignCode, GenericAlias, GenericAliasItem,
    GenericCItems, ModuleName, RTypeConvRule, TypeMapConvRuleInfo, DEFINE_C_TYPE, GENERIC_ALIAS,
    SWIG_CONCAT_IDENTS, SWIG_F_TYPE, SWIG_I_TYPE,
};
use crate::{
    source_registry::SourceId,
    str_replace::replace_first_and_other,
    typemap::{
        ast::{DisplayToTokens, SpannedSmolStr},
        TypeConvCode, FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE, TO_VAR_TYPE_TEMPLATE,
    },
    FOREIGNER_CODE, FOREIGN_CODE, FOREIGN_TYPEMAP,
};

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
        let mut main_span = None;

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
                let (rule, span) = if kw_la.peek(kw::r_type) {
                    let k = params.parse::<kw::r_type>()?;
                    let sp = k.span();
                    (RuleType::RType(k), sp)
                } else if kw_la.peek(kw::f_type) {
                    let k = params.parse::<kw::f_type>()?;
                    let sp = k.span();
                    (RuleType::FType(k), sp)
                } else {
                    return Err(kw_la.error());
                };
                if main_span.is_none() {
                    main_span = Some(span);
                }

                match rule {
                    RuleType::RType(keyword) => {
                        if !params.is_empty() {
                            return Err(params.error("extra paramaters for r_type rule"));
                        }
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
                        let (ftype_cfg, ftype_req_modules) =
                            parse_typemap_type_arm_param(&rule, &params)?;
                        parse_f_type_rule(
                            input,
                            var_name,
                            keyword,
                            &mut ftype_left_to_right,
                            &mut ftype_right_to_left,
                            ftype_cfg,
                            ftype_req_modules,
                        )?;
                    }
                }
            } else {
                let mac: syn::Macro = input.parse()?;
                if main_span.is_none() {
                    main_span = Some(mac.span());
                }
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
                    match syn::parse2::<CItems>(mac.tts.clone()) {
                        Ok(x) => c_types = Some(x),

                        Err(_) => generic_c_types = Some(syn::parse2::<GenericCItems>(mac.tts)?),
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

        let main_span =
            main_span.ok_or_else(|| input.error(format!("{} is empty", FOREIGN_TYPEMAP)))?;

        let rule = TypeMapConvRuleInfo {
            src_id: SourceId::none(),
            span: main_span,
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
                rule.generic_c_types.unwrap().types.span(),
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
        let out_var_no_type: TokenStream = parse_quote!($out_no_type);
        let out_var_no_type = out_var_no_type.to_string();

        let mut code_str = conv_body.to_string();
        if !(code_str.contains(&d_var_name)
            && (code_str.contains(&out_var) || code_str.contains(&out_var_no_type)))
        {
            return Err(syn::Error::new(
                conv_body.span(),
                format!(
                    "no $out or $out_no_type or ${} in conversation code",
                    var_name
                ),
            ));
        }
        code_str.push(';');
        Some(if code_str.contains(&out_var_no_type) {
            TypeConvCode::new2(
                code_str
                    .replace(&d_var_name, FROM_VAR_TEMPLATE)
                    .replace(&out_var_no_type, &format!("let {}", TO_VAR_TEMPLATE)),
                (SourceId::none(), conv_body.span()),
            )
        } else {
            TypeConvCode::new2(
                code_str.replace(&d_var_name, FROM_VAR_TEMPLATE).replace(
                    &out_var,
                    &format!("let {}: {}", TO_VAR_TEMPLATE, TO_VAR_TYPE_TEMPLATE),
                ),
                (SourceId::none(), conv_body.span()),
            )
        })
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

impl syn::parse::Parse for CItems {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<kw::module>()?;
        input.parse::<Token![=]>()?;
        let module_name: LitStr = input.parse()?;
        let header_name: SmolStr = module_name.value().into();
        input.parse::<Token![;]>()?;
        let citems_list: CItemsList = input.parse()?;
        Ok(CItems {
            header_name,
            items: citems_list.0,
        })
    }
}

pub(super) struct CItemsList(pub(super) Vec<CItem>);

impl syn::parse::Parse for CItemsList {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut types = vec![];
        while !input.is_empty() {
            let item: syn::Item = input.parse()?;
            match item {
                syn::Item::Struct(s) => {
                    if !has_repr_c_attr(&s.attrs) {
                        return Err(syn::Error::new(s.span(), "struct has no repr(C) attribute"));
                    }
                    types.push(CItem::Struct(s));
                }
                syn::Item::Union(u) => {
                    if !has_repr_c_attr(&u.attrs) {
                        return Err(syn::Error::new(u.span(), "union has no repr(C) attribute"));
                    }
                    types.push(CItem::Union(u));
                }
                syn::Item::Fn(f) => {
                    let mangle_attr: syn::Attribute = parse_quote! { #[no_mangle] };
                    if !f.attrs.iter().any(|a| *a == mangle_attr) {
                        return Err(syn::Error::new(
                            f.span(),
                            "fn has no #[no_mangle] attribute",
                        ));
                    }

                    match f.abi {
                        Some(ref abi) => match abi.name {
                            Some(ref name) if name.value() == "C" => {}
                            _ => {
                                return Err(syn::Error::new(
                                    abi.extern_token.span(),
                                    "fn marked as extern, but not extern \"C\"",
                                ))
                            }
                        },
                        None => {
                            return Err(syn::Error::new(f.span(), "fn not marked as extern \"C\""))
                        }
                    }

                    types.push(CItem::Fn(f));
                }
                _ => return Err(syn::Error::new(item.span(), "Expect struct or union here")),
            }
        }
        Ok(CItemsList(types))
    }
}

fn has_repr_c_attr(attrs: &[syn::Attribute]) -> bool {
    let repr_c_attr: syn::Attribute = parse_quote! { #[repr(C)] };
    attrs.iter().any(|a| *a == repr_c_attr)
}

impl syn::parse::Parse for ForeignCode {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<kw::module>()?;
        input.parse::<Token![=]>()?;
        let module_name: LitStr = input.parse()?;
        let sp = module_name.span();
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
            sp,
            module_name,
            cfg_option,
            code: code.value(),
        })
    }
}

impl syn::parse::Parse for GenericAlias {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let alias = input.parse()?;
        input.parse::<Token![=]>()?;
        let value = input.parse()?;
        Ok(GenericAlias { alias, value })
    }
}

impl syn::parse::Parse for GenericCItems {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        input.parse::<kw::module>()?;
        input.parse::<Token![=]>()?;
        let module_name: LitStr = input.parse()?;
        let header_name: SmolStr = module_name.value().into();
        input.parse::<Token![;]>()?;
        let types = input.parse()?;
        Ok(GenericCItems {
            header_name: ModuleName {
                name: header_name,
                sp: module_name.span(),
            },
            types,
        })
    }
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
            } else if mac.path.is_ident(SWIG_F_TYPE) {
                let item: syn::Ident = syn::parse2(mac.tts)?;
                Ok(GenericAliasItem::SwigFType(item))
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

fn parse_typemap_type_arm_param(
    rule: &RuleType,
    params: syn::parse::ParseStream,
) -> syn::Result<(Option<SpannedSmolStr>, Vec<ModuleName>)> {
    let mut ftype_cfg: Option<SpannedSmolStr> = None;
    let mut ftype_req_modules = Vec::<ModuleName>::new();

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
                let mod_name = modules.parse::<LitStr>()?;
                ftype_req_modules.push(ModuleName {
                    name: mod_name.value().into(),
                    sp: mod_name.span(),
                });
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

    Ok((ftype_cfg, ftype_req_modules))
}

fn parse_f_type_rule(
    input: syn::parse::ParseStream,
    var_name: Option<syn::Ident>,
    keyword: kw::f_type,
    ftype_left_to_right: &mut Vec<FTypeConvRule>,
    ftype_right_to_left: &mut Vec<FTypeConvRule>,
    ftype_cfg: Option<SpannedSmolStr>,
    ftype_req_modules: Vec<ModuleName>,
) -> syn::Result<()> {
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
            syn::Error::new(
                keyword.span(),
                "there is conversation code, but name of input variable not defined here",
            )
        })?;
        let var_name = format!("${}", var_name);
        Some(TypeConvCode::new(
            replace_first_and_other(
                code_str
                    .value()
                    .replace(&var_name, FROM_VAR_TEMPLATE)
                    .as_str(),
                "$out",
                TO_VAR_TYPE_TEMPLATE,
                TO_VAR_TEMPLATE,
            ),
            (SourceId::none(), code_str.span()),
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
    Ok(())
}
