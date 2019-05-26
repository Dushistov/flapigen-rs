use crate::typemap::{ty::FTypeConvCode, FROM_VAR_TEMPLATE};
use proc_macro2::Span;
use proc_macro2::TokenStream;
use smol_str::SmolStr;
use syn::{
    braced, parenthesized, parse_quote, spanned::Spanned, token, Ident, LitStr, Token, Type,
};

#[derive(Debug)]
pub(in crate::typemap) struct TypeMapConvRuleInfo {
    rtype_left_to_right: Option<RTypeConvRule>,
    rtype_right_to_left: Option<RTypeConvRule>,
    ftype_left_to_right: Option<FTypeConvRule>,
    ftype_right_to_left: Option<FTypeConvRule>,
}

#[derive(Debug, PartialEq)]
struct RTypeConvRule {
    left_ty: Type,
    right_ty: Option<Type>,
    code: Option<FTypeConvCode>,
}

#[derive(Debug, PartialEq)]
struct FTypeConvRule {
    left_ty: Option<FTypeName>,
    right_ty: Option<FTypeName>,
    code: Option<FTypeConvCode>,
}

#[derive(Debug)]
struct FTypeName {
    name: SmolStr,
    sp: Span,
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
}

enum RuleType {
    RType(kw::r_type),
    FType(kw::f_type),
}

enum ConvertRuleType<T> {
    LeftToRight(T),
    RightToLeft(T),
}

impl syn::parse::Parse for TypeMapConvRuleInfo {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut rtype_left_to_right: Option<RTypeConvRule> = None;
        let mut rtype_right_to_left: Option<RTypeConvRule> = None;
        let mut ftype_left_to_right: Option<FTypeConvRule> = None;
        let mut ftype_right_to_left: Option<FTypeConvRule> = None;

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

                let rule;
                let kw_la = params.lookahead1();
                if kw_la.peek(kw::r_type) {
                    rule = RuleType::RType(params.parse::<kw::r_type>()?);
                } else if kw_la.peek(kw::f_type) {
                    rule = RuleType::FType(params.parse::<kw::f_type>()?);
                } else {
                    return Err(kw_la.error());
                }
                match rule {
                    RuleType::RType(keyword) => {
                        let left_ty = input.parse::<Type>()?;
                        let mut conv_rule_type = None;
                        if input.peek(Token![=>]) {
                            input.parse::<Token![=>]>()?;
                            conv_rule_type =
                                Some(ConvertRuleType::LeftToRight(input.parse::<Type>()?));
                        } else if input.peek(Token![<=]) {
                            input.parse::<Token![<=]>()?;
                            conv_rule_type =
                                Some(ConvertRuleType::RightToLeft(input.parse::<Type>()?));
                        }

                        let mut code = None;
                        if conv_rule_type.is_some() && input.peek(syn::token::Brace) {
                            let content;
                            braced!(content in input);
                            let conv_body = content.parse::<TokenStream>()?;

                            let var_name = var_name.ok_or_else(|| {
                                syn::Error::new(keyword.span(), "there is conversation code, but name of input variable not defined here")
                            })?;
                            //because of $var most likely will be reformated to "$ var"
                            let var_name: TokenStream = parse_quote!($#var_name);
                            code = Some(FTypeConvCode::new(
                                conv_body
                                    .to_string()
                                    .replace(&var_name.to_string(), FROM_VAR_TEMPLATE),
                                conv_body.span(),
                            ));
                        }
                        match conv_rule_type {
                            Some(ConvertRuleType::LeftToRight(right_ty)) => {
                                if rtype_left_to_right.is_some() {
                                    return Err(syn::Error::new(
                                        keyword.span(),
                                        "duplicate of r_type left to right rule",
                                    ));
                                }
                                rtype_left_to_right = Some(RTypeConvRule {
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
                                rtype_right_to_left = Some(RTypeConvRule {
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
                                rtype_left_to_right = Some(RTypeConvRule {
                                    left_ty,
                                    right_ty: None,
                                    code: None,
                                });
                            }
                        }
                    }
                    RuleType::FType(keyword) => {
                        let mut left_ty = None;
                        if input.peek(LitStr) {
                            left_ty = Some(input.parse::<LitStr>()?.into());
                        }
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
                        let mut code = None;
                        if conv_rule_type.is_some() && input.peek(LitStr) {
                            let code_str = input.parse::<LitStr>()?;
                            let var_name = var_name.ok_or_else(|| {
                                syn::Error::new(keyword.span(), "there is conversation code, but name of input variable not defined here")
                            })?;
                            let var_name = format!("${}", var_name);
                            code = Some(FTypeConvCode::new(
                                code_str.value().replace(&var_name, FROM_VAR_TEMPLATE),
                                code_str.span(),
                            ));
                        }
                        match conv_rule_type {
                            Some(ConvertRuleType::LeftToRight(right_ty)) => {
                                if ftype_left_to_right.is_some() {
                                    return Err(syn::Error::new(
                                        keyword.span(),
                                        "duplicate of f_type left to right rule",
                                    ));
                                }
                                ftype_left_to_right = Some(FTypeConvRule {
                                    left_ty,
                                    right_ty: Some(right_ty),
                                    code,
                                });
                            }
                            Some(ConvertRuleType::RightToLeft(right_ty)) => {
                                if ftype_right_to_left.is_some() {
                                    return Err(syn::Error::new(
                                        keyword.span(),
                                        "duplicate of f_type right to left rule",
                                    ));
                                }
                                ftype_right_to_left = Some(FTypeConvRule {
                                    left_ty,
                                    right_ty: Some(right_ty),
                                    code,
                                });
                            }
                            None => {
                                if ftype_left_to_right.is_some() {
                                    return Err(syn::Error::new(
                                        keyword.span(),
                                        "duplicate of f_type left to right rule",
                                    ));
                                }
                                if left_ty.is_none() {
                                    return Err(syn::Error::new(
                                        keyword.span(),
                                        "expect type name in this kind of f_type rule",
                                    ));
                                }
                                ftype_left_to_right = Some(FTypeConvRule {
                                    left_ty,
                                    right_ty: None,
                                    code: None,
                                });
                            }
                        }
                    }
                }
            } else {
                let mac: syn::Macro = input.parse()?;
                input.parse::<Token![;]>()?;
            }
        }
        Ok(TypeMapConvRuleInfo {
            rtype_left_to_right,
            rtype_right_to_left,
            ftype_left_to_right,
            ftype_right_to_left,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::panic_on_syn_error;
    use syn::parse_quote;

    #[test]
    fn test_foreign_typemap_qdatetime() {
        let _rule = macro_to_conv_rule(parse_quote! {
            foreign_typemap!(
                ($pin:r_type) DateTime<Utc> => i64 {
                    $pin.timestamp()
                }
                ($pin:f_type) => "QDateTime" r#"
$out = QDateTime::fromMSecsSinceEpoch($pin * 1000, Qt::UTC, 0);
        "#
            )
        });
        /*
        assert_eq!("QDateTime", rule.f_type.name);
                assert_eq!(
                    "i64",
                    rule.f_type
                        .correspoding_rust_type
                        .into_token_stream()
                        .to_string()
                );*/
    }

    #[test]
    fn test_foreign_typemap_cpp_bool() {
        let rule = macro_to_conv_rule(parse_quote! {
            foreign_typemap!(
                ($pin:r_type) bool => ::std::os::raw::c_char {
                    if $pin  { 1 } else { 0 }
                }
                ($pin:f_type) => "bool" "$out = ($pin != 0);"
                ($pin:r_type) bool <= ::std::os::raw::c_char {
                    $pin != 0
                }
                ($pin:f_type) <= "bool" "$out = $pin ? 1 : 0"
            )
        });

        assert_eq!(
            RTypeConvRule {
                left_ty: parse_type!(bool),
                right_ty: Some(parse_type!(::std::os::raw::c_char)),
                code: Some(FTypeConvCode::new(
                    "if {from_var} { 1 } else { 0 }",
                    Span::call_site()
                )),
            },
            rule.rtype_left_to_right.unwrap()
        );

        assert_eq!(
            RTypeConvRule {
                left_ty: parse_type!(bool),
                right_ty: Some(parse_type!(::std::os::raw::c_char)),
                code: Some(FTypeConvCode::new("{from_var} != 0", Span::call_site())),
            },
            rule.rtype_right_to_left.unwrap()
        );

        assert_eq!(
            FTypeConvRule {
                left_ty: None,
                right_ty: Some(FTypeName {
                    name: "bool".into(),
                    sp: Span::call_site(),
                }),
                code: Some(FTypeConvCode::new(
                    "$out = ({from_var} != 0);",
                    Span::call_site()
                )),
            },
            rule.ftype_left_to_right.unwrap()
        );
    }

    #[test]
    fn test_foreign_typemap_qstring() {
        let rule = macro_to_conv_rule(parse_quote! {
            foreign_typemap!(
                ($rin:r_type) &str => RustStrView {
                    RustStrView::from_str($rin)
                }
                ($pin:f_type) => "QString" r#"
$out = QString::fromUtf8($pin.data, $pin.len);
"#)
        });
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
                    |x| $pin.cb(convert_to_c!(x), $pin.ctx)
                }

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
 }"#
            )
        });
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
                }
                ($pin:f_type) => "java.util.Date" "$out = new java.util.Date($pin);"
            )
        });
    }

    #[ignore]
    #[test]
    fn test_foreign_typemap_jstring() {
        let rule = macro_to_conv_rule(parse_quote! {
            foreign_typemap!(
                ($pin:r_type, $jstr:variable) &str <= jstring {
                    $jstr = JavaString::new(env, $pin);
                    $out = $jstr.to_str();
                }
                ($pin:f_type, non_null) <= "String"
            )
        });
    }

    fn macro_to_conv_rule(mac: syn::Macro) -> TypeMapConvRuleInfo {
        let _ = env_logger::try_init();
        let code = mac.tts.to_string();
        syn::parse_str::<TypeMapConvRuleInfo>(&code)
            .unwrap_or_else(|err| panic_on_syn_error("macro_to_conv_rule", code, err))
    }
}
