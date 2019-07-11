use log::debug;
use proc_macro2::{Ident, Span, TokenStream};
use rustc_hash::FxHashSet;
use smol_str::SmolStr;
use std::convert::{TryFrom, TryInto};
use syn::{
    braced, parenthesized,
    parse::{Parse, ParseStream},
    parse_quote,
    punctuated::Punctuated,
    spanned::Spanned,
    Token, Type,
};

use crate::{
    error::{DiagnosticError, Result},
    namegen::new_unique_name,
    source_registry::SourceId,
    typemap::ast::{normalize_ty_lifetimes, DisplayToTokens},
    types::{
        FnArg, ForeignEnumInfo, ForeignEnumItem, ForeignInterface, ForeignInterfaceMethod,
        ForeignerClassInfo, ForeignerMethod, MethodAccess, MethodVariant, NamedArg, SelfTypeDesc,
        SelfTypeVariant,
    },
    LanguageConfig, FOREIGNER_CODE, FOREIGN_CODE,
};

pub(crate) fn parse_foreigner_class(
    src_id: SourceId,
    config: &LanguageConfig,
    tokens: TokenStream,
) -> Result<ForeignerClassInfo> {
    match config {
        LanguageConfig::CppConfig(_) => {
            let mut class: CppClass =
                syn::parse2(tokens).map_err(|err| DiagnosticError::from_syn_err(src_id, err))?;
            class.0.src_id = src_id;
            Ok(class.0)
        }
        LanguageConfig::JavaConfig(_) => {
            let mut class: JavaClass =
                syn::parse2(tokens).map_err(|err| DiagnosticError::from_syn_err(src_id, err))?;
            class.0.src_id = src_id;
            Ok(class.0)
        }
        LanguageConfig::PythonConfig(_) => {
            let mut class: PythonClass = 
                syn::parse2(tokens).map_err(|err| DiagnosticError::from_syn_err(src_id, err))?;
            class.0.src_id = src_id;
            Ok(class.0)
        }
    }
}

pub(crate) fn parse_foreign_enum(src_id: SourceId, tokens: TokenStream) -> Result<ForeignEnumInfo> {
    let mut f_enum: ForeignEnumInfoParser =
        syn::parse2(tokens).map_err(|err| DiagnosticError::from_syn_err(src_id, err))?;
    f_enum.0.src_id = src_id;
    Ok(f_enum.0)
}

pub(crate) fn parse_foreign_interface(
    src_id: SourceId,
    tokens: TokenStream,
) -> Result<ForeignInterface> {
    let mut f_interface: ForeignInterfaceParser =
        syn::parse2(tokens).map_err(|err| DiagnosticError::from_syn_err(src_id, err))?;
    f_interface.0.src_id = src_id;
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

struct PythonClass(ForeignerClassInfo);

impl Parse for PythonClass {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(PythonClass(do_parse_foreigner_class(Language::Python, input)?))
    }
}

#[derive(Clone, Copy, PartialEq)]
enum Language {
    Cpp,
    Java,
    Python,
}

mod kw {
    use syn::custom_keyword;

    custom_keyword!(class);
    custom_keyword!(alias);
    custom_keyword!(private);
    custom_keyword!(protected);
    custom_keyword!(empty);
    custom_keyword!(interface);
    custom_keyword!(callback);
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
                        format!(
                            "Expect doc attribute or doc comment or derive here, got {}",
                            DisplayToTokens(&meta)
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
    } = parse_attrs(&input, true)?;
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
    let mut methods = Vec::with_capacity(10);

    static CONSTRUCTOR: &str = "constructor";
    static METHOD: &str = "method";
    static STATIC_METHOD: &str = "static_method";
    static FN: &str = "fn";

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
        let (func_type_name, func_type_name_span): (String, Span) = if content.peek(Token![fn]) {
            let token = content.parse::<Token![fn]>()?;
            (FN.into(), token.span())
        } else {
            let id: Ident = content.parse()?;
            (id.to_string(), id.span())
        };
        debug!("may be func_type_name {:?}", func_type_name);
        if func_type_name == "self_type" {
            rust_self_type = Some(content.parse::<Type>()?);
            debug!("self_type: {:?}", rust_self_type);
            content.parse::<Token![;]>()?;
            continue;
        }

        if func_type_name == FOREIGNER_CODE || func_type_name == FOREIGN_CODE {
            let lit: syn::LitStr = content.parse()?;
            debug!("foreigner_code {:?}", lit);
            foreigner_code.push_str(&lit.value());
            content.parse::<Token![;]>()?;
            continue;
        }

        if func_type_name == CONSTRUCTOR && has_dummy_constructor {
            return Err(syn::Error::new(
                func_type_name_span,
                "You defined dummy constructor for this, but have not dummy constructor",
            ));
        }

        if func_type_name == CONSTRUCTOR && content.peek(Token![=]) && content.peek2(kw::empty) {
            debug!("class {} has dummy constructor", class_name);
            content.parse::<Token![=]>()?;
            content.parse::<kw::empty>()?;
            if content.peek(Token![->]) {
                content.parse::<Token![->]>()?;
                let ret_type: Type = content.parse()?;
                debug!("constructor ret_ty {:?}", ret_type);
                constructor_ret_type = Some(ret_type);
            }
            content.parse::<Token![;]>()?;
            if access != MethodAccess::Private {
                return Err(content.error("dummy constructor should be private"));
            }
            if constructor_ret_type.is_none() {
                if let Some(rust_self_type) = rust_self_type.as_ref() {
                    let self_type: Type = (*rust_self_type).clone();
                    constructor_ret_type = Some(self_type);
                } else {
                    return Err(syn::Error::new(
                        class_name.span(),
                        "class has dummy constructor, but no self_type section",
                    ));
                }
            }

            let mut dummy_colon2: Token![::] = parse_quote! { :: };
            dummy_colon2.spans[0] = func_type_name_span;
            dummy_colon2.spans[1] = func_type_name_span;

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
                variant: MethodVariant::Constructor,
                rust_id: dummy_path,
                fn_decl: dummy_func.try_into()?,
                name_alias: None,
                inline_block: None,
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

        let mut func_type = match func_type_name {
            _ if func_type_name == CONSTRUCTOR => MethodVariant::Constructor,
            _ if func_type_name == STATIC_METHOD => MethodVariant::StaticMethod,
            _ if func_type_name == METHOD => MethodVariant::Method(SelfTypeVariant::Default),
            _ if func_type_name == FN => {
                if args_in.len() >= 1 {
                    use syn::FnArg::*;
                    match args_in[0] {
                        SelfRef(_) | SelfValue(_) => {
                            MethodVariant::Method(SelfTypeVariant::Default)
                        }
                        _ => MethodVariant::StaticMethod,
                    }
                } else {
                    MethodVariant::StaticMethod
                }
            }
            _ => {
                return Err(syn::Error::new(
                    func_type_name_span,
                    format!(
                        "expect 'constructor' or 'method' or \
                         'static_method' here, got: {}",
                        func_type_name
                    ),
                ));
            }
        };

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
                        DisplayToTokens(first_arg)
                    )));
                }
                None => {
                    return Err(content.error(
                        "No first argument in method (should be self/&self/&mut self/mut self)",
                    ));
                }
            },
        }
        let (fn_args, has_unnamed_args) = parse_fn_args(args_in)?;
        let out_type: syn::ReturnType = content.parse()?;
        debug!("out_type {:?}", out_type);

        let inline_block = if content.peek(syn::token::Brace) {
            let inline_body: syn::Block = content.parse()?;
            if has_unnamed_args {
                return Err(syn::Error::new(
                    func_type_name_span,
                    "there is unnamed argument, this is impossible for \"inline\" function",
                ));
            }
            Some(inline_body)
        } else {
            content.parse::<Token![;]>()?;
            None
        };

        let mut func_name_alias = None;
        if content.peek(kw::alias) {
            content.parse::<kw::alias>()?;
            if inline_block.is_some() {
                return Err(content.error("alias useless with \"inline\" function"));
            }
            if func_type == MethodVariant::Constructor {
                return Err(content.error("alias not supported for 'constructor'"));
            }
            func_name_alias = Some(content.parse::<syn::Ident>()?);
            debug!("we have ALIAS `{:?}`", func_name_alias);
            content.parse::<Token![;]>()?;
        }

        let ret_type = match out_type {
            syn::ReturnType::Default => None,
            syn::ReturnType::Type(_, ref ptype) => Some((*ptype).clone()),
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
                    return Err(syn::Error::new(
                        constructor_ret_type.span(),
                        format!(
                            "mismatched types of construtors: got {} expect {}",
                            DisplayToTokens(constructor_ret_type),
                            DisplayToTokens(&ret_type)
                        ),
                    ));
                }
            } else {
                debug!(
                    "first constructor for {}, ret type {:?}",
                    class_name, ret_type
                );
                constructor_ret_type = Some((*ret_type).clone());
            }
        }
        let span = func_name.span();
        methods.push(ForeignerMethod {
            variant: func_type,
            rust_id: func_name,
            fn_decl: crate::types::FnDecl {
                span,
                inputs: fn_args,
                output: out_type,
            },
            name_alias: func_name_alias,
            access,
            doc_comments,
            inline_block,
        });
    }

    let copy_derived = derive_list.iter().any(|x| x == "Copy");
    let clone_derived = derive_list.iter().any(|x| x == "Clone");
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

    let self_desc = match (rust_self_type, constructor_ret_type) {
        (Some(self_type), Some(constructor_ret_type)) => Some(SelfTypeDesc {
            self_type,
            constructor_ret_type,
        }),
        (None, None) => None,
        (Some(_), None) => {
            return Err(syn::Error::new(
                class_name.span(),
                "if self_type is defined you should add at least one constructor",
            ))
        }
        (None, Some(_)) => {
            return Err(syn::Error::new(
                class_name.span(),
                "there is constructor, you should define self_type",
            ))
        }
    };

    Ok(ForeignerClassInfo {
        src_id: SourceId::none(),
        name: class_name,
        methods,
        self_desc,
        foreigner_code,
        doc_comments: class_doc_comments,
        copy_derived,
        clone_derived,
    })
}

impl TryFrom<syn::FnDecl> for crate::types::FnDecl {
    type Error = syn::Error;
    fn try_from(x: syn::FnDecl) -> std::result::Result<Self, Self::Error> {
        Ok(crate::types::FnDecl {
            span: x.fn_token.span(),
            inputs: parse_fn_args(x.inputs)?.0,
            output: x.output,
        })
    }
}

pub(crate) fn parse_fn_args(
    args: Punctuated<syn::FnArg, Token![,]>,
) -> syn::Result<(Vec<FnArg>, bool)> {
    let mut has_unnamed_args = false;
    let mut ret = Vec::with_capacity(args.len());
    let invalid_arg = |sp| {
        Err(syn::Error::new(
            sp,
            "Invalid function argument, should be 'name: type' or '_: type' or 'type'",
        ))
    };
    let mut args_names = FxHashSet::<SmolStr>::default();
    for arg in args {
        use syn::FnArg::*;
        let fn_arg = match arg {
            SelfRef(syn::ArgSelfRef {
                self_token,
                ref mutability,
                ..
            }) => FnArg::SelfArg(
                self_token.span(),
                if mutability.is_some() {
                    SelfTypeVariant::RptrMut
                } else {
                    SelfTypeVariant::Rptr
                },
            ),
            SelfValue(syn::ArgSelf {
                self_token,
                ref mutability,
                ..
            }) => FnArg::SelfArg(
                self_token.span(),
                if mutability.is_some() {
                    SelfTypeVariant::Mut
                } else {
                    SelfTypeVariant::Default
                },
            ),
            Captured(syn::ArgCaptured { pat, ty, .. }) => {
                let (name, span): (SmolStr, Span) = match pat {
                    syn::Pat::Wild(w) => ("_".into(), w.span()),
                    syn::Pat::Ident(syn::PatIdent { ident, .. }) => {
                        (ident.to_string().into(), ident.span())
                    }
                    _ => return invalid_arg(pat.span()),
                };
                if name != "_" {
                    if args_names.contains(name.as_str()) {
                        return Err(syn::Error::new(
                            span,
                            format!("duplicate argument name '{}'", name),
                        ));
                    }
                    args_names.insert(name.clone());
                }
                FnArg::Default(NamedArg { name, ty, span })
            }
            Ignored(ty) => FnArg::Default(NamedArg {
                name: "_".into(),
                span: ty.span(),
                ty,
            }),
            _ => return invalid_arg(arg.span()),
        };
        ret.push(fn_arg);
    }

    for (i, named_arg) in ret
        .iter_mut()
        .filter_map(|x| match x {
            FnArg::SelfArg(_, _) => None,
            FnArg::Default(ref mut y) => Some(y),
        })
        .enumerate()
    {
        if named_arg.name == "_" {
            has_unnamed_args = true;
            let templ = format!("a{}", i);
            named_arg.name = new_unique_name(&args_names, &templ);
            debug_assert!(!args_names.contains(named_arg.name.as_str()));
            args_names.insert(named_arg.name.clone());
        }
    }
    Ok((ret, has_unnamed_args))
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
            src_id: SourceId::none(),
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
        let kw_la = input.lookahead1();
        if kw_la.peek(kw::interface) {
            input.parse::<kw::interface>()?;
        } else if kw_la.peek(kw::callback) {
            input.parse::<kw::callback>()?;
        } else {
            return Err(kw_la.error());
        }
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
            let have_self_args = match args_in.iter().nth(0) {
                Some(syn::FnArg::SelfRef(_)) => true,
                _ => false,
            };
            if !have_self_args {
                return Err(syn::Error::new(
                    rust_func_name.span(),
                    "expect &self or &mut self as first argument",
                ));
            }
            let fn_args = parse_fn_args(args_in)?.0;
            let out_type: syn::ReturnType = item_parser.parse()?;
            item_parser.parse::<Token![;]>()?;
            let span = rust_func_name.span();
            items.push(ForeignInterfaceMethod {
                name: func_name,
                rust_name: rust_func_name,
                fn_decl: crate::types::FnDecl {
                    span,
                    inputs: fn_args,
                    output: out_type,
                },
                doc_comments,
            });
        }

        let self_type: syn::Path = self_type.ok_or_else(|| {
            syn::Error::new(interface_name.span(), "No `self_type` in foreign_interface")
        })?;

        Ok(ForeignInterfaceParser(ForeignInterface {
            src_id: SourceId::none(),
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
    use crate::error::panic_on_syn_error;

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
                    method Foo::f(&self, _: i32, a: i32) -> i32;
                    static_method f2(_: i32, String) -> i32;
                    fn Foo::f3(&self) -> String;
                    fn Boo::f4(&self, a: i32) -> Vec<i32> {
                        let a = a + 1;
                        vec![a; self.n]
                    }
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
        let enum_ = parse_foreign_enum(SourceId::none(), mac.tts).unwrap();
        assert_eq!("MyEnum", enum_.name.to_string());
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
        let class: T =
            syn::parse2(tokens).unwrap_or_else(|err| panic_on_syn_error("test_parse", code, err));
        class
    }

}
