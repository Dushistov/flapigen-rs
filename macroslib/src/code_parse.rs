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
    ForeignEnumInfo, ForeignEnumItem, ForeignInterface, ForeignInterfaceMethod, ForeignerClassInfo,
    ForeignerMethod, LanguageConfig, MethodAccess, MethodVariant, SelfTypeVariant,
};

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

pub(crate) fn parse_foreign_enum(tokens: TokenStream) -> Result<ForeignEnumInfo> {
    let f_enum: ForeignEnumInfoParser = syn::parse2(tokens)?;
    Ok(f_enum.0)
}

pub(crate) fn parse_foreign_interface(tokens: TokenStream) -> Result<ForeignInterface> {
    let f_interface: ForeignInterfaceParser = syn::parse2(tokens)?;
    Ok(f_interface.0)
}

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

#[derive(Clone, Copy, PartialEq)]
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
    custom_keyword!(interface);
}

struct Attrs {
    doc_comments: Vec<String>,
    derive_list: Vec<String>,
}

fn parse_attrs(input: ParseStream, parse_derive_attrs: bool) -> syn::Result<Attrs> {
    let mut doc_comments = vec![];
    let mut derive_list = vec![];

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
                syn::Meta::List(syn::MetaList {
                    ref ident,
                    ref nested,
                    ..
                }) if ident == "derive" && parse_derive_attrs => {
                    for x in nested {
                        if let syn::NestedMeta::Meta(syn::Meta::Word(ref word)) = x {
                            derive_list.push(word.to_string());
                        } else {
                            return Err(syn::Error::new(x.span(), "Invalid derive format"));
                        }
                    }
                }
                _ => {
                    return Err(syn::Error::new(
                        a.span(),
                        &format!(
                            "Expect doc attribute or doc comment or derive here, got {:?}",
                            meta
                        ),
                    ));
                }
            }
        }
    }
    Ok(Attrs {
        doc_comments,
        derive_list,
    })
}

fn parse_doc_comments(input: ParseStream) -> syn::Result<Vec<String>> {
    let Attrs { doc_comments, .. } = parse_attrs(input, false)?;
    Ok(doc_comments)
}

fn do_parse_foreigner_class(lang: Language, input: ParseStream) -> syn::Result<ForeignerClassInfo> {
    let Attrs {
        doc_comments: class_doc_comments,
        derive_list,
    } = parse_attrs(&input, lang == Language::Cpp)?;
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
        let mut access = if content.peek(kw::private) {
            content.parse::<kw::private>()?;
            MethodAccess::Private
        } else {
            MethodAccess::Public
        };
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

            let mut dummy_colon2: Token![::] = parse_quote! { :: };
            dummy_colon2.spans[0] = func_type_name.span();
            dummy_colon2.spans[1] = func_type_name.span();

            let dummy_path = syn::Path {
                leading_colon: Some(dummy_colon2),
                segments: Punctuated::new(),
            };

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
        let func_name: syn::Path = content.call(syn::Path::parse_mod_style)?;
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
        let span = func_name.span();
        methods.push(ForeignerMethod {
            variant: func_type,
            rust_id: func_name,
            fn_decl: crate::FnDecl {
                span,
                inputs: args_in,
                output: out_type,
            },
            name_alias: func_name_alias,
            may_return_error,
            access,
            doc_comments,
        });
    }

    let copy_derived = derive_list.iter().any(|x| x == "Copy");
    let has_clone = |m: &ForeignerMethod| {
        if let Some(seg) = m.rust_id.segments.last() {
            let seg = seg.into_value();
            seg.ident == "clone"
        } else {
            false
        }
    };
    if copy_derived && !methods.iter().any(has_clone) {
        return Err(syn::Error::new(
            class_name.span(),
            "class marked as Copy, but no clone method",
        ));
    }

    Ok(ForeignerClassInfo {
        name: class_name,
        methods,
        self_type: rust_self_type.map(|x| x.clone().into()),
        this_type_for_method,
        foreigner_code,
        constructor_ret_type,
        doc_comments: class_doc_comments,
        copy_derived,
    })
}

struct ForeignEnumInfoParser(ForeignEnumInfo);

impl Parse for ForeignEnumInfoParser {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let enum_doc_comments = parse_doc_comments(input)?;
        input.parse::<Token![enum]>()?;
        let enum_name = input.parse::<Ident>()?;
        debug!("ENUM NAME {:?}", enum_name);
        let item_parser;
        braced!(item_parser in input);
        let mut items = vec![];
        while !item_parser.is_empty() {
            let doc_comments = parse_doc_comments(&item_parser)?;
            let f_item_name = item_parser.parse::<Ident>()?;
            item_parser.parse::<Token![=]>()?;
            let item_name = item_parser.call(syn::Path::parse_mod_style)?;
            item_parser.parse::<Token![,]>()?;

            items.push(ForeignEnumItem {
                name: f_item_name,
                rust_name: item_name,
                doc_comments,
            });
        }

        Ok(ForeignEnumInfoParser(ForeignEnumInfo {
            name: enum_name,
            items,
            doc_comments: enum_doc_comments,
        }))
    }
}

struct ForeignInterfaceParser(ForeignInterface);

impl Parse for ForeignInterfaceParser {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let interface_doc_comments = parse_doc_comments(input)?;
        input.parse::<kw::interface>()?;
        let interface_name = input.parse::<Ident>()?;
        debug!("INTERFACE NAME {:?}", interface_name);

        let item_parser;
        braced!(item_parser in input);

        let mut self_type = None;
        let mut items = vec![];

        while !item_parser.is_empty() {
            let doc_comments = parse_doc_comments(&item_parser)?;
            let func_name = item_parser.parse::<Ident>()?;
            if func_name == "self_type" {
                self_type = Some(item_parser.call(syn::Path::parse_mod_style)?);
                debug!("self_type: {:?} for {}", self_type, interface_name);
                item_parser.parse::<Token![;]>()?;
                continue;
            }
            item_parser.parse::<Token![=]>()?;
            let rust_func_name = item_parser.call(syn::Path::parse_mod_style)?;

            let args_parser;
            parenthesized!(args_parser in item_parser);
            let args_in: Punctuated<syn::FnArg, Token![,]> =
                args_parser.parse_terminated(syn::FnArg::parse)?;
            debug!("cb func in args {:?}", args_in);
            let out_type: syn::ReturnType = item_parser.parse()?;
            item_parser.parse::<Token![;]>()?;
            let span = rust_func_name.span();
            items.push(ForeignInterfaceMethod {
                name: func_name,
                rust_name: rust_func_name,
                fn_decl: crate::FnDecl {
                    span,
                    inputs: args_in,
                    output: out_type,
                },
                doc_comments,
            });
        }

        let self_type: syn::Path = self_type.ok_or_else(|| {
            syn::Error::new(interface_name.span(), "No `self_type` in foreign_interface")
        })?;

        Ok(ForeignInterfaceParser(ForeignInterface {
            name: interface_name,
            self_type,
            doc_comments: interface_doc_comments,
            items,
        }))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::{panic_on_parse_error, DiagnosticError};

    #[test]
    fn test_do_parse_foreigner_class() {
        let _ = env_logger::try_init();

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
        let java_class = test_parse::<JavaClass>(mac.tts);
        assert!(!java_class.0.copy_derived);

        let mac: syn::Macro = parse_quote! {
            foreigner_class!(class Foo {
                self_type SomeType;
                private constructor = empty;
                method SomeType::f(&self);
            })
        };
        test_parse::<JavaClass>(mac.tts);
        let mac: syn::Macro = parse_quote! {
            foreigner_class!(class Foo {
                self_type SomeType;
                private constructor = empty -> Box<OtherType>;
                method SomeType::f(&self); alias g;
            })
        };
        test_parse::<JavaClass>(mac.tts);
    }

    #[test]
    fn test_parse_foreign_enum() {
        let _ = env_logger::try_init();
        let mac: syn::Macro = parse_quote! {
            foreign_enum!(enum MyEnum {
                ITEM1 = MyEnum::Item1,
                ITEM2 = MyEnum::Item2,
                ITEM3 = MyEnum::Item3,
            })
        };
        let _enum = parse_foreign_enum(mac.tts).unwrap();
    }

    #[test]
    fn test_parse_foreign_class_with_copy_derive() {
        let _ = env_logger::try_init();
        let mac: syn::Macro = parse_quote! {
            foreigner_class!(#[derive(Copy)] class Foo {
                self_type SomeType;
                private constructor = empty;
                method SomeType::f(&self);
                method SomeType::clone(&self) -> SomeType;
            })
        };
        let class: CppClass = test_parse(mac.tts);
        assert!(class.0.copy_derived);
    }

    fn test_parse<T>(tokens: TokenStream) -> T
    where
        T: Parse,
    {
        let code = tokens.to_string();
        let class: T = syn::parse2(tokens).unwrap_or_else(|err| {
            let mut err: DiagnosticError = err.into();
            err.register_src("test_parse".into(), code);
            panic_on_parse_error(&err);
        });
        class
    }

}
