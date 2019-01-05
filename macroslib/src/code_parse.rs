use log::debug;
use proc_macro2::{Ident, TokenStream};
use quote::{quote, ToTokens};
use syn::{
    braced, parenthesized,
    parse::{Parse, ParseStream},
    parse_quote,
    punctuated::Punctuated,
    spanned::Spanned,
    Token, Type,
};

use crate::{
    ast::{if_result_return_ok_err_types, normalize_ty_lifetimes},
    error::Result,
    ForeignerClassInfo, ForeignerMethod, LanguageConfig, MethodAccess, MethodVariant,
    SelfTypeVariant,
};

struct CppClass(ForeignerClassInfo);

impl Parse for CppClass {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(CppClass(do_parse_foreigner_class(Language::Cpp, input)?))
    }
}

struct JavaClass(ForeignerClassInfo);

impl Parse for JavaClass {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(JavaClass(do_parse_foreigner_class(Language::Java, input)?))
    }
}

pub(crate) fn parse_foreigner_class(
    config: &LanguageConfig,
    tokens: TokenStream,
) -> Result<ForeignerClassInfo> {
    match config {
        LanguageConfig::CppConfig(_) => {
            let class: CppClass = syn::parse2(tokens)?;
            Ok(class.0)
        }
        LanguageConfig::JavaConfig(_) => {
            let class: JavaClass = syn::parse2(tokens)?;
            Ok(class.0)
        }
    }
}

enum Language {
    Cpp,
    Java,
}

mod kw {
    use syn::custom_keyword;

    custom_keyword!(class);
    custom_keyword!(alias);
    custom_keyword!(private);
    custom_keyword!(protected);
    custom_keyword!(empty);
}

fn parse_doc_comments(input: &ParseStream) -> syn::Result<Vec<String>> {
    let mut doc_comments = vec![];

    if input.fork().call(syn::Attribute::parse_outer).is_ok() {
        let attr: Vec<syn::Attribute> = input.call(syn::Attribute::parse_outer)?;
        for a in attr {
            let meta = a.parse_meta()?;
            match meta {
                syn::Meta::NameValue(syn::MetaNameValue {
                    ref ident,
                    lit: syn::Lit::Str(ref lit_str),
                    ..
                }) if ident == "doc" => {
                    doc_comments.push(lit_str.value());
                }
                _ => {
                    return Err(syn::Error::new(
                        a.span(),
                        "Expect doc attribute or doc comment here",
                    ));
                }
            }
        }
    }
    Ok(doc_comments)
}

fn do_parse_foreigner_class(lang: Language, input: ParseStream) -> syn::Result<ForeignerClassInfo> {
    let class_doc_comments = parse_doc_comments(&input)?;
    debug!(
        "parse_foreigner_class: class comment {:?}",
        class_doc_comments
    );

    input.parse::<kw::class>()?;
    let class_name: Ident = input.parse()?;
    debug!("class_name {:?}", class_name);
    let content;
    braced!(content in input);

    let mut rust_self_type = None;
    let mut foreigner_code = String::new();
    let mut has_dummy_constructor = false;
    let mut constructor_ret_type: Option<Type> = None;
    let mut this_type_for_method: Option<Type> = None;
    let mut methods = Vec::with_capacity(10);

    static CONSTRUCTOR: &str = "constructor";
    static METHOD: &str = "method";
    static STATIC_METHOD: &str = "static_method";

    while !content.is_empty() {
        let doc_comments = parse_doc_comments(&&content)?;
        let mut access = MethodAccess::Public;
        if content.peek(kw::private) {
            content.parse::<kw::private>()?;
            access = MethodAccess::Private;
        }
        if let Language::Cpp = lang {
            if content.peek(kw::protected) {
                content.parse::<kw::protected>()?;
                access = MethodAccess::Protected;
            }
        }
        let func_type_name: Ident = content.parse()?;
        debug!("may be func_type_name {:?}", func_type_name);
        if func_type_name == "self_type" {
            rust_self_type = Some(content.parse::<Type>()?);
            debug!("self_type: {:?}", rust_self_type);
            content.parse::<Token![;]>()?;
            continue;
        }

        if func_type_name == "foreigner_code" {
            let lit: syn::LitStr = content.parse()?;
            debug!("foreigner_code {:?}", lit);
            foreigner_code.push_str(&lit.value());
            content.parse::<Token![;]>()?;
            continue;
        }

        let mut func_type = match func_type_name {
            _ if func_type_name == CONSTRUCTOR => {
                if has_dummy_constructor {
                    return Err(syn::Error::new(
                        func_type_name.span(),
                        "You defined dummy constructor for this, but have not dummy constructor",
                    ));
                }
                MethodVariant::Constructor
            }
            _ if func_type_name == STATIC_METHOD => MethodVariant::StaticMethod,
            _ if func_type_name == METHOD => MethodVariant::Method(SelfTypeVariant::Default),
            _ => {
                return Err(syn::Error::new(
                    func_type_name.span(),
                    format!(
                        "expect 'constructor' or 'method' or \
                         'static_method' here, got: {}",
                        func_type_name
                    ),
                ));
            }
        };
        if func_type == MethodVariant::Constructor
            && content.peek(Token![=])
            && content.peek2(kw::empty)
        {
            debug!("class {} has dummy constructor", class_name);
            content.parse::<Token![=]>()?;
            content.parse::<kw::empty>()?;
            if content.peek(Token![->]) {
                content.parse::<Token![->]>()?;
                let ret_type: Type = content.parse()?;
                debug!("constructor ret_ty {:?}", ret_type);
                constructor_ret_type = Some(ret_type.clone());
                this_type_for_method = Some(
                    if_result_return_ok_err_types(constructor_ret_type.as_ref().unwrap())
                        .unwrap_or_else(|| (ret_type.clone(), ret_type))
                        .0,
                );
            }
            content.parse::<Token![;]>()?;
            if access != MethodAccess::Private {
                return Err(content.error("dummy constructor should be private"));
            }
            if this_type_for_method.is_none() {
                if let Some(rust_self_type) = rust_self_type.as_ref() {
                    let self_type: Type = (*rust_self_type).clone();
                    this_type_for_method = Some(self_type.clone());
                    constructor_ret_type = Some(self_type);
                } else {
                    return Err(syn::Error::new(
                        class_name.span(),
                        "class has dummy constructor, but no self_type section",
                    ));
                }
            }
            let dummy_path = syn::LitStr::new(&func_type_name.to_string(), func_type_name.span())
                .parse::<syn::Path>()?;
            let dummy_func: syn::ItemFn = parse_quote! {
                fn constructor() {
                }
            };
            let dummy_func = *dummy_func.decl;
            methods.push(ForeignerMethod {
                variant: func_type,
                rust_id: dummy_path,
                fn_decl: dummy_func.into(),
                name_alias: None,
                may_return_error: false,
                access,
                doc_comments,
            });
            has_dummy_constructor = true;
            continue;
        }
        let func_name: syn::Path = content.parse()?;
        debug!("func_name {:?}", func_name);

        //just skip <'a,...> section
        if content.fork().parse::<syn::Generics>().is_ok() {
            let _generics: syn::Generics = content.parse()?;
        }
        let args_parser;
        parenthesized!(args_parser in content);
        let args_in: Punctuated<syn::FnArg, Token![,]> =
            args_parser.parse_terminated(syn::FnArg::parse)?;
        debug!("func in args {:?}", args_in);
        match func_type {
            MethodVariant::Constructor | MethodVariant::StaticMethod => {
                let have_self_args = args_in.iter().any(|x| {
                    use syn::FnArg::*;
                    match x {
                        SelfRef(_) | SelfValue(_) => true,
                        Captured(_) | Inferred(_) | Ignored(_) => false,
                    }
                });
                if have_self_args {
                    return Err(content
                        .error("constructor or static_method should not contain self argument"));
                }
            }
            MethodVariant::Method(ref mut self_type) => match args_in.iter().nth(0) {
                Some(syn::FnArg::SelfRef(syn::ArgSelfRef { ref mutability, .. })) => {
                    *self_type = if mutability.is_some() {
                        SelfTypeVariant::RptrMut
                    } else {
                        SelfTypeVariant::Rptr
                    };
                }

                Some(syn::FnArg::SelfValue(syn::ArgSelf { ref mutability, .. })) => {
                    *self_type = if mutability.is_some() {
                        SelfTypeVariant::Mut
                    } else {
                        SelfTypeVariant::Default
                    };
                }
                Some(first_arg) => {
                    return Err(content.error(format!(
                        "Can not parse type {} as self type",
                        first_arg.into_token_stream().to_string()
                    )));
                }
                None => {
                    return Err(content.error(
                        "No first argument in method (should be self/&self/&mut self/mut self)",
                    ));
                }
            },
        }
        let out_type: syn::ReturnType = content.parse()?;
        debug!("out_type {:?}", out_type);
        content.parse::<Token![;]>()?;

        let mut func_name_alias = None;
        if content.peek(kw::alias) {
            content.parse::<kw::alias>()?;
            if func_type == MethodVariant::Constructor {
                return Err(content.error("alias not supported for 'constructor'"));
            }
            func_name_alias = Some(content.parse::<syn::Ident>()?);
            debug!("we have ALIAS `{:?}`", func_name_alias);
            content.parse::<Token![;]>()?;
        }

        let (may_return_error, ret_type) = match out_type {
            syn::ReturnType::Default => (false, None),
            syn::ReturnType::Type(_, ref ptype) => (
                if_result_return_ok_err_types(&*ptype).is_some(),
                Some((*ptype).clone()),
            ),
        };
        if func_type == MethodVariant::Constructor {
            let ret_type = match ret_type {
                Some(x) => x,
                None => {
                    return Err(
                        content.error(format!("{}: constructor should return value", class_name))
                    );
                }
            };
            if let Some(ref constructor_ret_type) = constructor_ret_type {
                debug!("second constructor, ret type: {:?}", constructor_ret_type);
                if normalize_ty_lifetimes(constructor_ret_type)
                    != normalize_ty_lifetimes(&*ret_type)
                {
                    return Err(content.error(format!(
                        "mismatched types of construtors: {:?} {:?}",
                        constructor_ret_type, ret_type
                    )));
                }
            } else {
                debug!(
                    "first constructor for {}, ret type {:?}",
                    class_name, ret_type
                );
                constructor_ret_type = Some((*ret_type).clone());
                this_type_for_method = Some(
                    if_result_return_ok_err_types(constructor_ret_type.as_ref().unwrap())
                        .unwrap_or_else(|| ((*ret_type).clone(), *ret_type))
                        .0,
                );
            }
        }
        methods.push(ForeignerMethod {
            variant: func_type,
            rust_id: func_name,
            fn_decl: crate::FnDecl {
                inputs: args_in,
                output: out_type,
            },
            name_alias: func_name_alias,
            may_return_error: may_return_error,
            access,
            doc_comments,
        });
    }

    Ok(ForeignerClassInfo {
        name: class_name,
        methods,
        self_type: rust_self_type.map(|x| x.clone().into()),
        this_type_for_method,
        foreigner_code,
        constructor_ret_type,
        doc_comments: class_doc_comments,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::{report_parse_error, DiagnosticError};

    #[test]
    fn test_do_parse_foreigner_class() {
        let _ = env_logger::try_init();

        let test_parse = |tokens: TokenStream| {
            let code = tokens.to_string();
            let class: JavaClass = syn::parse2(tokens).unwrap_or_else(|err| {
                let err: DiagnosticError = err.into();
                report_parse_error("test_do_parse_foreigner_class", &code, &err);
            });
            class.0
        };

        let mac: syn::Macro = parse_quote! {
            foreigner_class!(
                /// This is Foo :)
                /// Another doc comment
                class Foo {
                    self_type Foo;
                            foreigner_code r#"
        void f() {}
        "#;
                    constructor Foo::new(_: i32) -> Foo;
                    method Foo::set_field(&mut self, _: i32);
                    method Foo::f(&self, _: i32, _: i32) -> i32;
                    static_method f2(_: i32, String) -> i32;
                })
        };
        test_parse(mac.tts);

        let mac: syn::Macro = parse_quote! {
            foreigner_class!(class Foo {
                self_type SomeType;
                private constructor = empty;
                method SomeType::f(&self);
            })
        };
        test_parse(mac.tts);
        let mac: syn::Macro = parse_quote! {
            foreigner_class!(class Foo {
                self_type SomeType;
                private constructor = empty -> Box<OtherType>;
                method SomeType::f(&self); alias g;
            })
        };
        test_parse(mac.tts);
    }
}
