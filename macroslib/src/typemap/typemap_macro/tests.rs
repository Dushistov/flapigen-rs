use super::*;
use crate::{
    error::panic_on_syn_error,
    typemap::{FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE, TO_VAR_TYPE_TEMPLATE},
};
use quote::quote;
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
    assert!(!rule.if_simple_rtype_ftype_map_no_lang_backend().is_some());
    assert!(!rule.contains_data_for_language_backend());
}

#[test]
fn test_foreign_typemap_simple_generic() {
    let rule = macro_to_conv_rule(parse_quote! {
        foreign_typemap!(
            (r_type) <T: SwigTypeIsReprC> *const T;
            (f_type) "const swig_i_type!(T) *";
        )
    });
    assert!(rule.is_generic());
    assert!(rule.if_simple_rtype_ftype_map_no_lang_backend().is_some());
    assert!(rule.contains_data_for_language_backend());
    let ty = parse_type! { * const u32 };
    let subst_map =
        rule.is_ty_subst_of_my_generic_rtype(&ty, petgraph::Direction::Outgoing, |_ty, _traits| {
            true
        });
    assert!(subst_map.is_some());
    let ty = parse_type! { * const ::std::os::raw::c_char };
    let subst_map =
        rule.is_ty_subst_of_my_generic_rtype(&ty, petgraph::Direction::Outgoing, |_ty, _traits| {
            true
        });
    assert!(subst_map.is_some());
}

#[test]
fn test_foreign_typemap_cpp_bool() {
    let rule = macro_to_conv_rule(parse_quote! {
        foreign_typemap!(
            ($pin:r_type) bool => ::std::os::raw::c_char {
                $out = if $pin  { 1 } else { 0 };
            };
            ($pin:f_type) => "bool" "$out = ($pin != 0);";
            ($pin:r_type) bool <= ::std::os::raw::c_char {
                $out = ($pin != 0);
            };
            ($pin:f_type) <= "bool" "$out = $pin ? 1 : 0;";
        )
    });
    println!("rule {:?}", rule);
    assert!(!rule.is_generic());
    assert!(!rule.if_simple_rtype_ftype_map_no_lang_backend().is_some());
    assert!(!rule.contains_data_for_language_backend());

    assert_eq!(
        RTypeConvRule {
            left_ty: parse_type!(bool),
            right_ty: Some(parse_type!(::std::os::raw::c_char)),
            code: Some(TypeConvCode::new(
                "let mut {to_var}: {to_var_type} = if {from_var} { 1 } else { 0 } ;",
                invalid_src_id_span(),
            )),
        },
        rule.rtype_left_to_right.unwrap()
    );

    assert_eq!(
        RTypeConvRule {
            left_ty: parse_type!(bool),
            right_ty: Some(parse_type!(::std::os::raw::c_char)),
            code: Some(TypeConvCode::new(
                quote! { let mut out = (inp != 0); }
                    .to_string()
                    .replace(
                        "out",
                        &format!("{}: {}", TO_VAR_TEMPLATE, TO_VAR_TYPE_TEMPLATE)
                    )
                    .replace("inp", FROM_VAR_TEMPLATE),
                invalid_src_id_span(),
            )),
        },
        rule.rtype_right_to_left.unwrap()
    );

    assert_eq!(
        vec![FTypeConvRule {
            unique_prefix: None,
            input_to_output: false,
            req_modules: vec![],
            left_right_ty: FTypeLeftRightPair::OnlyRight(FTypeName {
                name: "bool".into(),
                sp: Span::call_site(),
            }),
            code: Some(TypeConvCode::new(
                "{to_var_type} = ({from_var} != 0);",
                invalid_src_id_span(),
            )),
            cfg_option: None,
        }],
        rule.ftype_left_to_right,
    );

    assert_eq!(
        vec![FTypeConvRule {
            unique_prefix: None,
            input_to_output: false,
            req_modules: vec![],
            left_right_ty: FTypeLeftRightPair::OnlyRight(FTypeName {
                name: "bool".into(),
                sp: Span::call_site(),
            }),
            code: Some(TypeConvCode::new(
                "{to_var_type} = {from_var} ? 1 : 0;",
                invalid_src_id_span(),
            )),
            cfg_option: None,
        }],
        rule.ftype_right_to_left,
    );
}

#[test]
fn test_foreign_typemap_qstring() {
    let rule = macro_to_conv_rule(parse_quote! {
        foreign_typemap!(
            ($rin:r_type) &str => RustStrView {
                $out = RustStrView::from_str($rin);
            };
            ($pin:f_type) => "QString" r#"
$out = QString::fromUtf8($pin.data, $pin.len);
"#;)
    });
    assert!(!rule.if_simple_rtype_ftype_map_no_lang_backend().is_some());
    assert!(!rule.contains_data_for_language_backend());
}

#[test]
fn test_foreign_typemap_bare_fn() {
    let rule = macro_to_conv_rule(parse_quote! {
        foreign_typemap!(
            generic_alias!(CFnTwoArgsPtr = swig_concat_idents!(c_fn_, swig_i_type!(T1), swig_i_type!(T1), _t));
            foreign_code!(
                module = "CFnTwoArgsPtr!().h";
                r##"
typedef void (*CFnTwoArgsPtr!())(swig_f_type!(T1), swig_f_type!(T2));
"##
            );
            (r_type) <T1: SwigTypeIsReprC, T2: SwigTypeIsReprC> extern "C" fn(T1, T2);
            (f_type, req_modules = ["\"CFnTwoArgsPtr!().h\""]) "CFnTwoArgsPtr!()";
        )

    });
    assert!(!rule.if_simple_rtype_ftype_map_no_lang_backend().is_some());
    assert!(rule.contains_data_for_language_backend());
    assert!(rule.is_generic());
    assert!(rule.if_simple_rtype_ftype_map().is_some());

    let ty = parse_type! { extern "C" fn(i32, f32)  };
    let subst_map = rule
        .is_ty_subst_of_my_generic_rtype(&ty, petgraph::Direction::Incoming, |_ty, _traits| true)
        .unwrap();
    assert_eq!(2, subst_map.len());
    assert_eq!(parse_type! { i32 }, *subst_map.get("T1").unwrap().unwrap());
    assert_eq!(parse_type! { f32 }, *subst_map.get("T2").unwrap().unwrap());
    println!("rule.generic_alias {:?}", rule.generic_aliases);
    let new_rule = rule
        .subst_generic_params(subst_map, petgraph::Direction::Incoming, &mut Dummy)
        .unwrap();
    assert!(!new_rule.is_generic());
    assert!(!new_rule.is_empty());
}

#[test]
fn test_foreign_typemap_callback_to_future() {
    let rule = macro_to_conv_rule(parse_quote! {
        foreign_typemap!(
            generic_alias!(CFnOnce = swig_concat_idents!(CFnOnce, swig_i_type!(T, output)));
            define_c_type!(
                module = "CFnOnce!().h";
                #[repr(C)]
                struct CFnOnce!() {
                    cb: extern "C" fn(swig_i_type!(T, output), *mut c_void),
                    ctx: *mut c_void,
                });

            ($p:r_type) <T> impl FnOnce(T) <= CFnOnce!()
            {
                $out = |x| {
                    swig_from_rust_to_i_type!(T, x, x);
                    $p.cb(x, $p.ctx);
                };
            };

            ($p:f_type, $tmp:temporary, input_to_output, req_modules = ["\"CFnOnce!().h\"", "<future>"]) <= "std::future<swig_f_type!(T, output)>"
                r#"
        auto $tmp = new std::promise<swig_f_type!(T, output)>;
        $out = $tmp->get_future();
        CFnOnce!() $p;
        $p.ctx = $tmp;
        $p.cb = [](swig_i_type!(T) arg, void *opaque) {
            auto arg_cpp = swig_foreign_from_i_type!(T, arg);
            auto promise = static_cast<std::promise<swig_f_type!(T, output)> *>(opaque);
            promise->set_value(std::move(arg_cpp));
            delete promise;
        };
"#;
        )
    });
    assert!(!rule.if_simple_rtype_ftype_map_no_lang_backend().is_some());
    assert!(rule.contains_data_for_language_backend());
    assert!(rule.is_generic());
    assert!(rule.ftype_right_to_left[0].input_to_output);

    let ty = parse_type! { impl FnOnce(u32) };
    let subst_map = rule
        .is_ty_subst_of_my_generic_rtype(&ty, petgraph::Direction::Incoming, |_ty, _traits| true)
        .unwrap();
    assert_eq!(1, subst_map.len());
    assert_eq!(parse_type! { u32 }, *subst_map.get("T").unwrap().unwrap());
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
            ($pin:f_type, option = "NoNullAnnotation") => "java.util.Date" "$out = new java.util.Date($pin);";
            ($p:f_type, option = "NullAnnotation") => "@NonNull java.util.Date" "$out = new java.util.Date($p);";
        )
    });
    assert!(rule.if_simple_rtype_ftype_map_no_lang_backend().is_none());
    assert!(rule.contains_data_for_language_backend());
}

#[test]
fn test_foreign_typemap_jboolean() {
    let rule = macro_to_conv_rule(parse_quote! {
        foreign_typemap!(
            ($p:r_type) bool => jboolean {
                $out = if $p { 1 as jboolean } else { 0 as jboolean };
            };
            ($p:f_type) => "boolean";
            ($p:r_type) bool <= jboolean {
                $out = ($p != 0);
            };
            ($p:f_type) <= "boolean";
        )
    });
    assert!(!rule.is_empty());
    assert!(!rule.is_generic());
    assert!(!rule.if_simple_rtype_ftype_map().is_some());
    assert!(!rule.if_simple_rtype_ftype_map_no_lang_backend().is_some());
    assert!(!rule.contains_data_for_language_backend());
}

#[test]
fn test_foreign_typemap_simple_typemap() {
    let rule = macro_to_conv_rule(parse_quote! {
        foreign_typemap!(
            (r_type) jlong;
            (f_type) "long";
        )
    });
    assert!(rule.if_simple_rtype_ftype_map_no_lang_backend().is_some());
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
            unique_prefix: None,
            input_to_output: false,
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
                $out = CRustString::from_string($pin);
            };
            ($pin:f_type, req_modules = ["rust_str.h"]) => "RustString" "RustString{$pin}";
        )
    });
    assert!(!rule.if_simple_rtype_ftype_map_no_lang_backend().is_some());
    assert!(rule.contains_data_for_language_backend());
    assert_eq!(
        CItems {
            header_name: "rust_str.h".into(),
            items: vec![CItem::Struct(parse_quote! {
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
            $out = CRustStrView::from_str($p);
        };
        ($p:f_type, option = "CppStrView::Boost", req_modules = ["\"rust_str.h\"", "<boost/utility/string_view.hpp>"]) => "boost::string_view"
            "boost::string_view{ $p.data, $p.len }";
        ($p:f_type, option = "CppStrView::Std17", req_modules = ["\"rust_str.h\"", "<string_view>"]) => "std::string_view"
            "std::string_view{ $p.data, $p.len }";
    )
            });
    assert!(!rule.if_simple_rtype_ftype_map_no_lang_backend().is_some());
    assert!(rule.contains_data_for_language_backend());
}

#[test]
fn test_foreign_typemap_cpp_pair_expand() {
    let rule = cpp_pair_rule();
    println!("rule!!!: {:?}", rule);
    assert!(rule.is_generic());
    let subst_params = rule
        .is_ty_subst_of_my_generic_rtype(&parse_type! {(i32, f32)}, Direction::Outgoing, |_, _| {
            false
        })
        .unwrap();
    let c_types = rule
        .subst_generic_params_to_c_items(&subst_params, &mut Dummy)
        .unwrap()
        .unwrap();

    assert_eq!(
        CItems {
            header_name: "rust_tuple.h".into(),
            items: vec![CItem::Struct(parse_quote! {
                #[repr(C)]
                pub struct CRustPairi32f32 {
                    first: i32,
                    second: f32,
                }
            }),],
        },
        c_types,
    );

    let new_rule = rule
        .subst_generic_params(subst_params, petgraph::Direction::Outgoing, &mut Dummy)
        .unwrap();
    assert!(!new_rule.is_generic());
    assert!(!new_rule.contains_data_for_language_backend());
}

#[test]
fn test_foreign_typemap_cpp_pair_syntax() {
    let rule = cpp_pair_rule();
    println!("rule!!!: {:?}", rule);
    assert!(!rule.if_simple_rtype_ftype_map_no_lang_backend().is_some());
    assert!(rule.contains_data_for_language_backend());
    assert!(rule.is_generic());

    assert!(rule
        .is_ty_subst_of_my_generic_rtype(&parse_type! {i32}, Direction::Outgoing, |_, _| false,)
        .is_none());
    assert!(rule
        .is_ty_subst_of_my_generic_rtype(&parse_type! {()}, Direction::Outgoing, |_, _| false,)
        .is_none());

    let generics: syn::Generics = parse_quote! { <T1, T2> };
    assert_eq!(generics, *rule.rtype_generics.as_ref().unwrap());
    assert_eq!(
        RTypeConvRule {
            left_ty: parse_type! {(T1, T2)},
            right_ty: Some(parse_type! { CRustPair!() }),
            code: Some(TypeConvCode::new(
                quote!(
                    swig_from_rust_to_i_type!(T1, inp.0, p0);
                    swig_from_rust_to_i_type!(T2 , inp.1, p1);
                    let mut out = CRustPair!() { first: p0, second: p1, };
                )
                .to_string()
                .replace(
                    "out",
                    &format!("{}: {}", TO_VAR_TEMPLATE, TO_VAR_TYPE_TEMPLATE)
                )
                .replace("inp", FROM_VAR_TEMPLATE),
                invalid_src_id_span(),
            )),
        },
        *rule.rtype_left_to_right.as_ref().unwrap()
    );
    assert_eq!(
        RTypeConvRule {
            left_ty: parse_type! { (T1, T2) },
            right_ty: Some(parse_type! { CRustPair!() }),
            code: Some(TypeConvCode::new(
                quote!(
                    swig_from_i_type_to_rust!(T1, inp.first, p0);
                    swig_from_i_type_to_rust!(T2, inp.second, p1);
                    let mut out = (p0, p1);
                )
                .to_string()
                .replace(
                    "out",
                    &format!("{}: {}", TO_VAR_TEMPLATE, TO_VAR_TYPE_TEMPLATE)
                )
                .replace("inp", FROM_VAR_TEMPLATE),
                invalid_src_id_span(),
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
        rule.is_ty_subst_of_my_generic_rtype(
            &parse_type! {(i32, f32)},
            Direction::Outgoing,
            |_, _| false,
        )
    );
}

#[test]
fn test_foreign_typemap_unique_prefix() {
    let rule = macro_to_conv_rule(parse_quote! {
        foreign_typemap!(
            ($p:r_type) Option<&str> <= jstring {
                let tmp: JavaString;
                $out = if !$p.is_null() {
                    tmp = $p.swig_into(env);
                    Some(tmp.swig_deref())
                } else {
                    None
                };
            };
            ($p:f_type, option = "NoNullAnnotations", unique_prefix = "/*opt*/") <= "/*opt*/String";
            ($p:f_type, option = "NullAnnotations", unique_prefix = "/*opt*/") <= "/*opt*/@Nullable String";
        )
    });
    assert!(!rule.is_empty());
    assert_eq!(None, rule.if_simple_rtype_ftype_map());
    assert_eq!(None, rule.if_simple_rtype_ftype_map_no_lang_backend());
    assert!(rule.contains_data_for_language_backend());
    assert!(!rule.is_generic());
}

#[test]
fn test_expand_generic_type_with_ptr() {
    let rule = macro_to_conv_rule(parse_quote! {
        foreign_typemap!(
            ($p:r_type) <T> Option<&T> <= *const swig_i_type!(T) {
                $out = if !$p.is_null() {
                    Some(o)
                } else {
                    None
                };
            };
            ($p:f_type, unique_prefix = "/*opt ref*/") <= "/*opt ref*/const swig_f_type!(T) *" r#"
                $out = ($p != nullptr) ? static_cast<const swig_i_type!(T) *>(* $p) : nullptr;
"#;
        )
    });
    println!("rule {:?}", rule);
    assert!(!rule.is_empty());
    assert!(rule.is_generic());

    let ty = parse_type! { Option<&u32> };
    let subst_map = rule
        .is_ty_subst_of_my_generic_rtype(&ty, petgraph::Direction::Incoming, |_ty, _traits| true)
        .unwrap();
    assert_eq!(1, subst_map.len());
    assert_eq!(parse_type! { u32 }, *subst_map.get("T").unwrap().unwrap());

    let new_rule = rule
        .subst_generic_params(subst_map, petgraph::Direction::Incoming, &mut Dummy)
        .unwrap();
    assert!(!new_rule.is_generic());
    assert!(!new_rule.is_empty());
    assert_eq!(
        RTypeConvRule {
            left_ty: parse_type!(Option<&u32>),
            right_ty: Some(parse_type!(*const u32)),
            code: Some(TypeConvCode::new(
                quote! { let mut out = if !inp.is_null() { Some (o) } else { None }; }
                    .to_string()
                    .replace(
                        "out",
                        &format!("{}: {}", TO_VAR_TEMPLATE, TO_VAR_TYPE_TEMPLATE)
                    )
                    .replace("inp", FROM_VAR_TEMPLATE),
                invalid_src_id_span(),
            )),
        },
        new_rule.rtype_right_to_left.unwrap()
    );
}

#[test]
fn test_java_c_like_enum_generic() {
    let rule = macro_to_conv_rule(parse_quote! {
        foreign_typemap!(
            ($p:r_type) <T: SwigForeignCLikeEnum> T => jint {
                $out = $p.as_jint();
            };
        )
    });
    println!("rule: {:?}", rule);
    assert!(rule.is_generic());

    let _: GenericTypeConv = macro_to_conv_rule(parse_quote! {
        foreign_typemap!(
            ($p:r_type) <T: SwigForeignCLikeEnum> T => jint {
                $out = $p.as_jint();
            };
        )
    })
    .try_into()
    .unwrap();
    let ty = parse_type! { MyEnum };
    let subst_map =
        rule.is_ty_subst_of_my_generic_rtype(&ty, petgraph::Direction::Outgoing, |_ty, _traits| {
            true
        });
    assert!(subst_map.is_some());
    let subst_map =
        rule.is_ty_subst_of_my_generic_rtype(&ty, petgraph::Direction::Outgoing, |_ty, _traits| {
            false
        });
    assert!(!subst_map.is_some());
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
                };
            };
            ($p:r_type) <T1, T2> (T1, T2) <= CRustPair!() {
                swig_from_i_type_to_rust!(T1, $p.first, p0);
                swig_from_i_type_to_rust!(T2, $p.second, p1);
                $out = (p0, p1);
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
    let code = mac.tokens.to_string();
    syn::parse_str::<TypeMapConvRuleInfo>(&code)
        .unwrap_or_else(|err| panic_on_syn_error("macro_to_conv_rule", code, err))
}

struct Dummy;
impl TypeMapConvRuleInfoExpanderHelper for Dummy {
    fn swig_i_type(&mut self, ty: &syn::Type, _opt_arg: Option<&str>) -> Result<syn::Type> {
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
    fn swig_f_type(&mut self, ty: &syn::Type, _: Option<&str>) -> Result<ExpandedFType> {
        Ok(ExpandedFType {
            name: if *ty == parse_type!(i32) {
                "int32_t"
            } else if *ty == parse_type!(u32) {
                "uint32_t"
            } else if *ty == parse_type!(f32) {
                "float"
            } else if *ty == parse_type!(CRustPairi32f32) {
                "CRustPairi32f32"
            } else {
                panic!("swig_f_type: Unknown type: {}", DisplayToTokens(ty));
            }
            .into(),
            provided_by_module: vec![],
        })
    }
    fn swig_foreign_to_i_type(&mut self, _ty: &syn::Type, var_name: &str) -> Result<String> {
        Ok(var_name.into())
    }
    fn swig_foreign_from_i_type(&mut self, ty: &syn::Type, var_name: &str) -> Result<String> {
        self.swig_foreign_to_i_type(ty, var_name)
    }
}
