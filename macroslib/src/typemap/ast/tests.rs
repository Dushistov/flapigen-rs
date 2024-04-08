use super::*;
use proc_macro2::LineColumn;
use smallvec::smallvec;
use syn::spanned::Spanned;

#[test]
fn test_normalize_ty() {
    assert_eq!(
        parse_type! { & str },
        str_to_ty(normalize_type(&parse_type! { &str })),
    );
    assert_eq!(
        parse_type! { & str },
        str_to_ty(normalize_type(&parse_type! { &'a str })),
    );
    assert_eq!(
        parse_type! { string },
        str_to_ty(normalize_type(&parse_type! { string })),
    );
    assert_eq!(
        parse_type! { () },
        str_to_ty(normalize_type(&parse_type! { () })),
    );
    assert_eq!(
        parse_type! { Foo < T > },
        str_to_ty(normalize_type(&parse_type! { Foo<'a, T> })),
    );
    assert_eq!(
        parse_type! { Foo },
        str_to_ty(normalize_type(&parse_type! { Foo<'a> }))
    );
    assert_eq!(
        parse_type! { Box < Trait > },
        str_to_ty(normalize_type(&parse_type! { Box<dyn Trait> })),
    );
}

macro_rules! get_generic_params_from_code {
        ($($tt:tt)*) => {{
            let item: syn::ItemImpl = parse_quote! { $($tt)* };
            item.generics
        }}
    }

#[test]
fn generic_type_conv_find() {
    let _ = env_logger::try_init();
    let generic = get_generic_params_from_code! {
        #[swig_to_foreigner_hint = "T []"]
        impl<T: SwigForeignClass> SwigFrom<Vec<T>> for jobjectArray {
            fn swig_from(x: Vec<T>, env: *mut JNIEnv) -> Self {
                vec_of_objects_to_jobject_array(x, <T>::jni_class_name(), env)
            }
        }
    };

    let foo_spec = Rc::new(
        RustTypeS::new_without_graph_idx(str_to_ty("Foo"), "Foo", SourceId::none())
            .implements("SwigForeignClass"),
    );

    let refcell_foo_spec = Rc::new(
        RustTypeS::new_without_graph_idx(
            str_to_ty("RefCell<Foo>"),
            "RefCell<Foo>",
            SourceId::none(),
        )
        .implements("SwigForeignClass"),
    );

    fn check_subst<'a, FT: Fn(&str) -> Option<&'a RustType>>(
        generic: &syn::Generics,
        from_ty_name: &str,
        to_ty_name: &str,
        ty_check_name: &str,
        expect_to_ty_name: &str,
        map_others: FT,
    ) -> RustType {
        println!(
            "check_subst: conv {} -> {} with {}",
            from_ty_name, to_ty_name, ty_check_name
        );
        let ConversionResult {
            to_ty: ret_ty,
            to_ty_name: ret_ty_name,
            ..
        } = GenericTypeConv::new(
            str_to_ty(from_ty_name),
            str_to_ty(to_ty_name),
            generic.clone(),
            TypeConvCode::invalid(),
        )
        .is_conv_possible(&str_to_rust_ty(ty_check_name), None, map_others)
        .expect("check subst failed");
        assert_eq!(ret_ty_name, normalize_type(&str_to_ty(expect_to_ty_name)));

        Rc::new(RustTypeS::new_without_graph_idx(
            ret_ty,
            ret_ty_name,
            SourceId::none(),
        ))
    }

    let pair_generic = get_generic_params_from_code! {
        impl<T1: SwigForeignClass, T2: SwigForeignClass> SwigFrom<(T1, T2)> for CRustObjectPair {
            fn swig_from((x1, x2): (T1, T2)) -> Self {
                unimplemented!();
            }
        }
    };

    let one_spec = Rc::new(
        RustTypeS::new_without_graph_idx(str_to_ty("One"), "One", SourceId::none())
            .implements("SwigForeignClass"),
    );
    let two_spec = Rc::new(
        RustTypeS::new_without_graph_idx(str_to_ty("One"), "One", SourceId::none())
            .implements("SwigForeignClass"),
    );
    check_subst(
        &pair_generic,
        "(T1, T2)",
        "CRustObjectPair",
        "(One, Two)",
        "CRustObjectPair",
        |name| {
            println!("test pair map, check name {:?}", name);
            if name == "One" {
                Some(&one_spec)
            } else if name == "Two" {
                Some(&two_spec)
            } else {
                None
            }
        },
    );

    check_subst(
        &generic,
        "Rc<T>",
        "jlong",
        "Rc<RefCell<Foo>>",
        "jlong",
        |name| {
            println!("test rt map, check name {:?}", name);
            if name == "Foo" {
                Some(&foo_spec)
            } else if name == "RefCell < Foo >" {
                Some(&refcell_foo_spec)
            } else {
                None
            }
        },
    );

    check_subst(
        &generic,
        "Vec<T>",
        "jobjectArray",
        "Vec<Foo>",
        "jobjectArray",
        |name| {
            if name == "Foo" {
                Some(&foo_spec)
            } else {
                None
            }
        },
    );

    let generic = get_generic_params_from_code! {
        impl<'a, T> SwigFrom<&'a RefCell<T>> for RefMut<'a, T> {
            fn swig_from(m: &'a RefCell<T>, _: *mut JNIEnv) -> RefMut<'a, T> {
                m.borrow_mut()
            }
        }
    };

    check_subst(
        &generic,
        "&RefCell<T>",
        "RefMut<T>",
        "&RefCell<Foo>",
        "RefMut<Foo>",
        |_| None,
    );

    check_subst(
        &generic,
        "&Rc<T>",
        "&T",
        "&Rc<RefCell<Foo>>",
        "&RefCell<Foo>",
        |_| None,
    );

    check_subst(
        &generic,
        "Arc<Mutex<T>>",
        "&Mutex<T>",
        "Arc<Mutex<Foo>>",
        "&Mutex<Foo>",
        |_| None,
    );

    let mutex_guard_foo = check_subst(
        &generic,
        "&Mutex<T>",
        "MutexGuard<T>",
        "&Mutex<Foo>",
        "MutexGuard<Foo>",
        |_| None,
    );
    assert_eq!(
        &*GenericTypeConv::new(
            str_to_ty("MutexGuard<T>"),
            str_to_ty("&T"),
            generic.clone(),
            TypeConvCode::invalid(),
        )
        .is_conv_possible(&mutex_guard_foo, None, |name| if name == "Foo" {
            Some(&foo_spec)
        } else {
            None
        })
        .unwrap()
        .to_ty_name,
        "& Foo"
    );

    let box_foo: RustType = str_to_rust_ty("Box<Foo>");

    assert_eq!(
        &*GenericTypeConv::new(
            str_to_ty("jlong"),
            str_to_ty("Box<T>"),
            generic,
            TypeConvCode::invalid(),
        )
        .is_conv_possible(&str_to_rust_ty("jlong"), Some(&box_foo), |_| None)
        .unwrap()
        .to_ty_name,
        "Box < Foo >"
    );

    let generic = get_generic_params_from_code! {
        impl<T: SwigForeignClass> SwigFrom<Box<T>> for jlong {
            fn swig_from(x: Box<T>, _: *mut JNIEnv) -> jlong {
                unimplemented!();
            }
        }
    };
    check_subst(&generic, "T", "Box<T>", "Foo", "Box<Foo>", |name| {
        if name == "Foo" {
            Some(&foo_spec)
        } else {
            None
        }
    });

    let generic = get_generic_params_from_code! {
        impl<T, E> SwigFrom<Result<T,E>> for T {
            fn swig_from(v: Result<T, E>, _: *mut JNIEnv) -> T {
                unimplemented!();
            }
        }
    };
    check_subst(
        &generic,
        "Result<T, E>",
        "T",
        "Result<u8, &'static str>",
        "u8",
        |_| None,
    );
}

#[test]
fn test_get_trait_bounds() {
    let _ = env_logger::try_init();

    assert_eq!(
        get_trait_bounds(&get_generic_params_from_code! {
            impl<T> Foo for Boo {}
        }),
        GenericTraitBoundVec::new(),
    );

    let moo_path: syn::Path = parse_quote! { Moo };

    assert_eq!(
        get_trait_bounds(&get_generic_params_from_code! {
            impl<T: Moo> Foo for Boo {}
        }),
        {
            let mut trait_names = TraitNamesSet::default();
            trait_names.insert(&moo_path);
            let v: GenericTraitBoundVec = smallvec![GenericTraitBound {
                ty_param: TyParamRef::Own(Ident::new("T", Span::call_site())),
                trait_names,
            }];
            v
        }
    );

    assert_eq!(
        get_trait_bounds(&get_generic_params_from_code! {
            impl<T> Foo for Boo where T: Moo {}
        }),
        {
            let mut trait_names = TraitNamesSet::default();
            trait_names.insert(&moo_path);
            let v: GenericTraitBoundVec = smallvec![GenericTraitBound {
                ty_param: TyParamRef::Own(Ident::new("T", Span::call_site())),
                trait_names,
            }];
            v
        }
    );
}

#[test]
fn test_work_with_option() {
    assert_eq!(
        "String",
        normalize_type(&if_option_return_some_type(&str_to_rust_ty("Option<String>")).unwrap())
    );
}

#[test]
fn test_work_with_result() {
    assert_eq!(
        if_result_return_ok_err_types(&str_to_rust_ty("Result<bool, String>"))
            .map(|(x, y)| (normalize_type(&x), normalize_type(&y)))
            .unwrap(),
        ("bool", "String")
    );

    assert_eq!(
        if_ty_result_return_ok_type(&str_to_ty("Result<bool, String>"))
            .map(|x| normalize_type(&x))
            .unwrap(),
        "bool"
    );

    assert_eq!(
        if_ty_result_return_ok_type(&str_to_ty("Result<Option<i32>, String>"))
            .map(|x| normalize_type(&x))
            .unwrap(),
        "Option < i32 >"
    );
}

#[test]
fn test_work_with_rc() {
    let ty = check_if_smart_pointer_return_inner_type(&str_to_rust_ty("Rc<RefCell<bool>>"), "Rc")
        .unwrap();
    assert_eq!("RefCell < bool >", normalize_type(&ty));

    let generic_params: syn::Generics = parse_quote! { <T> };
    assert_eq!(
        "bool",
        GenericTypeConv::new(
            str_to_ty("RefCell<T>"),
            str_to_ty("T"),
            generic_params,
            TypeConvCode::invalid(),
        )
        .is_conv_possible(&str_to_rust_ty(normalize_type(&ty)), None, |_| None)
        .unwrap()
        .to_ty_name
    );
}

#[test]
fn test_replace_all_types_with() {
    let t_ident: Ident = parse_quote! { T };
    let e_ident: Ident = parse_quote! { E };
    assert_eq!(
        {
            let ty: Type = parse_quote! { & Vec<T> };
            ty
        },
        replace_all_types_with(&parse_quote! { &T }, &{
            let mut subst_map = TyParamsSubstMap::default();
            subst_map.insert(&t_ident, Some(parse_quote! { Vec<T> }));
            subst_map
        })
    );

    assert_eq!(
        {
            let ty: Type = parse_quote! { Result<i32, String> };
            ty
        },
        replace_all_types_with(&parse_quote! { Result<T, E> }, &{
            let mut subst_map = TyParamsSubstMap::default();
            subst_map.insert(&t_ident, Some(parse_quote! { i32 }));
            subst_map.insert(&e_ident, Some(parse_quote! { String }));
            subst_map
        })
    );
}

#[test]
fn test_list_lifetimes() {
    let my_list_lifetimes = |code| -> Vec<String> {
        let ty = str_to_ty(code);
        let ret = list_lifetimes(&ty);
        ret.iter().map(|v| v.ident.to_string()).collect()
    };
    assert_eq!(vec!["a"], my_list_lifetimes("Rc<RefCell<Foo<'a>>>"));
}

#[test]
fn test_is_second_subst_of_first_span() {
    let ty1: Type = syn::parse_str(
        r#"
Result<T, E>
"#,
    )
    .unwrap();
    assert_eq!(LineColumn { line: 2, column: 0 }, ty1.span().start());
    assert_eq!(
        LineColumn {
            line: 2,
            column: 12
        },
        ty1.span().end()
    );
    let ty2: Type = syn::parse_str(
        r#"

Result<u16, u8>
"#,
    )
    .unwrap();
    assert_eq!(LineColumn { line: 3, column: 0 }, ty2.span().start());
    assert_eq!(
        LineColumn {
            line: 3,
            column: 15
        },
        ty2.span().end()
    );
    let t_id: Ident = parse_quote! { T };
    let e_id: Ident = parse_quote! { E };
    {
        let mut subst_map = TyParamsSubstMap::default();
        subst_map.insert(&t_id, None);
        subst_map.insert(&e_id, None);
        assert!(is_second_subst_of_first(&ty1, &ty2, &mut subst_map));
        let t_ty = subst_map.get(&t_id).unwrap().unwrap();
        assert_eq!(parse_type! {u16}, *t_ty);
        assert_eq!(LineColumn { line: 3, column: 7 }, t_ty.span().start());
        assert_eq!(
            LineColumn {
                line: 3,
                column: 10
            },
            t_ty.span().end()
        );
        assert_eq!(parse_type! {u8}, *subst_map.get(&e_id).unwrap().unwrap());
    }
}

#[test]
fn test_jlong_to_option_wrong_conv() {
    let _ = env_logger::try_init();

    let generics = get_generic_params_from_code! {
        impl<T: SwigForeignClass> SwigFrom<jlong> for Option<T> {
            fn swig_from(x: jlong, _: *mut JNIEnv) -> Self {
                if x != 0 {
                    let o: T = T::unbox_object(x);
                    Some(o)
                } else {
                    None
                }
            }
        }
    };

    let foo_spec = Rc::new(
        RustTypeS::new_without_graph_idx(str_to_ty("Foo"), "Foo", SourceId::none())
            .implements("SwigForeignClass"),
    );
    assert_eq!(
        None,
        GenericTypeConv::new(
            str_to_ty("jlong"),
            str_to_ty("Option<T>"),
            generics,
            TypeConvCode::invalid(),
        )
        .is_conv_possible(
            &str_to_rust_ty("jlong"),
            Some(&str_to_rust_ty("&Foo")),
            |name| {
                println!("test rt map, check name {:?}", name);
                if name == "Foo" {
                    Some(&foo_spec)
                } else {
                    None
                }
            },
        )
    );

    let generics = get_generic_params_from_code! {
        impl<T: SwigForeignClass + Clone> SwigInto<Vec<T>> for jobjectArray {
            fn swig_into(self, env: *mut JNIEnv) -> Vec<T> {
                unimplemented!();
            }
        }
    };

    assert_eq!(
        None,
        GenericTypeConv::new(
            str_to_ty("jobjectArray"),
            str_to_ty("Vec<T>"),
            generics,
            TypeConvCode::invalid(),
        )
        .is_conv_possible(
            &str_to_rust_ty("jobjectArray"),
            Some(&str_to_rust_ty("Vec<Foo>")),
            |name| {
                println!("test rt map, check name {:?}", name);
                if name == "Foo" {
                    Some(&foo_spec)
                } else {
                    None
                }
            },
        )
    );
}

#[test]
fn test_is_second_subst_of_first_pointer() {
    let _ = env_logger::try_init();
    let generics: syn::Generics = parse_quote! { <T> };
    let mut subst_map = TyParamsSubstMap::default();
    for ty_p in generics.type_params() {
        subst_map.insert(&ty_p.ident, None);
    }
    let ty = parse_type! { *const u32 };
    let generic_ty = parse_type! { *const T };
    assert!(is_second_subst_of_first(&generic_ty, &ty, &mut subst_map));
    assert_eq!(1, subst_map.len());
    assert_eq!(parse_type! { u32 }, *subst_map.get("T").unwrap().unwrap());
}

#[test]
fn test_is_second_subst_of_first_char_pointer() {
    let _ = env_logger::try_init();
    let generics: syn::Generics = parse_quote! { <T> };
    let mut subst_map = TyParamsSubstMap::default();
    for ty_p in generics.type_params() {
        subst_map.insert(&ty_p.ident, None);
    }
    let ty = parse_type! { *const ::std::os::raw::c_char };
    let generic_ty = parse_type! { *const T };
    assert!(is_second_subst_of_first(&generic_ty, &ty, &mut subst_map));
    assert_eq!(1, subst_map.len());
    assert_eq!(
        parse_type! { ::std::os::raw::c_char },
        *subst_map.get("T").unwrap().unwrap()
    );
}

#[test]
fn test_is_second_subst_of_first_impl_fnonce_no_ret() {
    let _ = env_logger::try_init();
    let generics: syn::Generics = parse_quote! { <T> };
    let mut subst_map = TyParamsSubstMap::default();
    for ty_p in generics.type_params() {
        subst_map.insert(&ty_p.ident, None);
    }
    let ty = parse_type! { impl FnOnce(u32) };
    let generic_ty = parse_type! { impl FnOnce(T) };
    assert!(is_second_subst_of_first(&generic_ty, &ty, &mut subst_map));
    assert_eq!(1, subst_map.len());
    assert_eq!(parse_type! { u32 }, *subst_map.get("T").unwrap().unwrap());
}

#[test]
fn test_is_second_subst_of_first_impl_fnonce_with_ret() {
    let _ = env_logger::try_init();
    let generics: syn::Generics = parse_quote! { <T, U> };
    let mut subst_map = TyParamsSubstMap::default();
    for ty_p in generics.type_params() {
        subst_map.insert(&ty_p.ident, None);
    }
    let ty = parse_type! { impl FnOnce(u32) -> String };
    let generic_ty = parse_type! { impl FnOnce(T) -> U };
    assert!(is_second_subst_of_first(&generic_ty, &ty, &mut subst_map));
    assert_eq!(2, subst_map.len());
    assert_eq!(parse_type! { u32 }, *subst_map.get("T").unwrap().unwrap());
    assert_eq!(
        parse_type! { String },
        *subst_map.get("U").unwrap().unwrap()
    );
}

#[test]
fn test_is_second_subst_of_first_extern_c_fn_ptr() {
    let _ = env_logger::try_init();
    let generics: syn::Generics = parse_quote! { <T, U> };
    let mut subst_map = TyParamsSubstMap::default();
    for ty_p in generics.type_params() {
        subst_map.insert(&ty_p.ident, None);
    }
    let ty = parse_type! { extern "C" fn(i32,f32) };
    let generic_ty = parse_type! { extern "C" fn(T,U) };
    assert!(is_second_subst_of_first(&generic_ty, &ty, &mut subst_map));
    assert_eq!(2, subst_map.len());
    assert_eq!(parse_type! { i32 }, *subst_map.get("T").unwrap().unwrap());
    assert_eq!(parse_type! { f32 }, *subst_map.get("U").unwrap().unwrap());
}

#[test]
fn test_is_second_subst_of_first_tuple() {
    let _ = env_logger::try_init();
    let generics: syn::Generics = parse_quote! { <T1, T2> };
    let mut subst_map = TyParamsSubstMap::default();
    for ty_p in generics.type_params() {
        subst_map.insert(&ty_p.ident, None);
    }

    let ty = parse_type! { (i32, f32) };
    let generic_ty = parse_type! { (T1, T2) };
    assert!(is_second_subst_of_first(&generic_ty, &ty, &mut subst_map));
    assert_eq!(2, subst_map.len());
    assert_eq!(parse_type! { i32 }, *subst_map.get("T1").unwrap().unwrap());
    assert_eq!(parse_type! { f32 }, *subst_map.get("T2").unwrap().unwrap());

    let ty = parse_type! { (&str, &str) };
    let generic_ty = parse_type! { (T1, T2) };
    subst_map = TyParamsSubstMap::default();
    for ty_p in generics.type_params() {
        subst_map.insert(&ty_p.ident, None);
    }
    assert!(is_second_subst_of_first(&generic_ty, &ty, &mut subst_map));
    assert_eq!(2, subst_map.len());
    assert_eq!(parse_type! { &str }, *subst_map.get("T1").unwrap().unwrap());
    assert_eq!(parse_type! { &str }, *subst_map.get("T2").unwrap().unwrap());
}

#[test]
fn test_parse_type_spanned_macro() {
    let ty = str_to_ty(" Cow<str> ");

    assert_eq!(LineColumn { line: 1, column: 1 }, ty.span().start());
    assert_eq!(LineColumn { line: 1, column: 9 }, ty.span().end());

    let span = ty.span();
    let ty2: syn::Type = parse_type_spanned_checked!(span, & #ty);
    assert_eq!(parse_type! { & Cow<str> }, ty2);

    assert_eq!(LineColumn { line: 1, column: 1 }, ty2.span().start());
    assert_eq!(LineColumn { line: 1, column: 9 }, ty2.span().end());
}

fn str_to_ty(code: &str) -> syn::Type {
    syn::parse_str::<syn::Type>(code).unwrap()
}

fn str_to_rust_ty(code: &str) -> RustType {
    let ty = syn::parse_str::<syn::Type>(code).unwrap();
    let name = normalize_type(&ty);
    Rc::new(RustTypeS::new_without_graph_idx(ty, name, SourceId::none()))
}
