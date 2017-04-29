use std::cell::RefCell;
use std::rc::Rc;
use std::mem;

use syntex_syntax::ext::base::ExtCtxt;
use syntex_syntax::{parse, ast};
use syntex_syntax::codemap::Spanned;
use syntex_syntax::print::pprust;

use core::{TypeHandler, FromForeignArgConverter, Converter, ToForeignRetConverter};
use {unpack_first_associated_type, path_match, type_name_is_deref_of,
     path_unpack_genearic_first_parameter};

pub fn parse_types_map(cx: &mut ExtCtxt, jni_types_map_code: &str) -> Vec<TypeHandler> {
    let mut parser = parse::new_parser_from_source_str(cx.parse_sess,
                                                       "jni_types_map".into(),
                                                       jni_types_map_code.into());
    let mut my_crate = parser.parse_crate_mod().unwrap();
    let mut type_handlers = Vec::new();

    for mut item in my_crate.module.items.iter_mut() {

        let mut foreigner_type = None;

        for (idx, it) in item.attrs.iter().enumerate() {
            let attr_name: &str = &it.value.name.as_str();
            if attr_name != "foreigner_type" {
                continue;
            }
            if let ast::MetaItemKind::NameValue(Spanned {
                                                    node: ast::LitKind::Str(lit, _),
                                                    span: _,
                                                }) = it.value.node {
                foreigner_type = Some((idx, lit.as_str().to_string()));
                break;
            }
        }

        let (foreigner_type_idx, foreigner_type) = match foreigner_type {
            Some((x, y)) => (x, y),
            None => {
                debug!("{:?}", *item);
                if let ast::ItemKind::Impl(ast::Unsafety::Normal,
                                           ast::ImplPolarity::Positive,
                                           _,
                                           Some(ref trait_type),
                                           ref for_type,
                                           ref impl_items) = item.node {
                    if path_match(&trait_type.path, "Deref") {
                        let target = unpack_first_associated_type(impl_items, "Target")
                            .expect("Can not get Target for Deref");
                        debug!("target {:?}, for_type {:?}", target, for_type);
                        let deref_target = pprust::ty_to_string(&target);
                        let th_pos_main = type_handlers.iter().position(|v: &TypeHandler|
                            type_name_is_deref_of(&v.rust_type_name, &deref_target)
                        ).expect("Can not find Deref(main) match type in type map");
                        let for_typename = pprust::ty_to_string(&for_type);
                        let th_pos_inter = type_handlers.iter().position(|v: &TypeHandler| {
                            v.rust_type_name == for_typename
                        }).expect("Can not find Deref(inter) match type in type map");
                        let mut depends = vec![Rc::new(RefCell::new(Converter::new(item.clone())))];
                        type_handlers[th_pos_inter].from_jni_converter
                            .as_ref()
                            .map(|x| {
                                for dep in &x.depends {
                                    depends.push(dep.clone());
                                }
                            });
                        type_handlers[th_pos_main].from_jni_converter =
                            Some(FromForeignArgConverter {
                                code: format!(r#"
    let {{arg_name}}: {} = {{arg_name}}.swig_into(env);
    let {{arg_name}} = &{{arg_name}};
"#, for_typename),
                                depends: depends,
                            });
                    } else {
                        panic!("Internal Error: unknown trait in type maps {:?}, segs {:?}",
                               trait_type.path, trait_type.path.segments);
                    }
                }
                continue;
            }
        };

        let mut new_item = item.clone()
            .map(|mut v| {
                     v.attrs.remove(foreigner_type_idx);
                     v
                 });
        mem::swap(&mut *item, &mut new_item);

        if let ast::ItemKind::Impl(ast::Unsafety::Normal,
                                   ast::ImplPolarity::Positive,
                                   _,
                                   Some(ref trait_type),
                                   ref for_type,
                                   _) = item.node {
            let is_into = path_match(&trait_type.path, "SwigInto");
            let is_from = path_match(&trait_type.path, "SwigFrom");
            if is_into || is_from {
                let rust_type =
                    path_unpack_genearic_first_parameter(&trait_type.path,
                                                         if is_into { "SwigInto" }
                                                         else { "SwigFrom" })
                    .unwrap();
                debug!("java type: {:?}, jni type: {:?}, rust_type {:?}",
                         foreigner_type, for_type, rust_type);
                let rust_type_name = pprust::ty_to_string(&rust_type);
                let th_pos = type_handlers.iter().position(|v: &TypeHandler|
                                                           v.rust_type_name == rust_type_name);
                let th_pos = if let Some(th_pos) = th_pos {
                    th_pos
                } else {
                    type_handlers.push(TypeHandler {
                        rust_type_name: rust_type_name,
                        jni_type_name: pprust::ty_to_string(for_type),
                        java_type_name: foreigner_type,
                        from_jni_converter: None,
                        to_jni_converter: None,
                    });
                    type_handlers.len() - 1
                };
                if is_into {
                    type_handlers[th_pos].from_jni_converter =
                        Some(FromForeignArgConverter {
                            code: "let {arg_name} = {arg_name}.swig_into(env);".into(),
                            depends: vec![Rc::new(RefCell::new(Converter::new(item.clone())))],
                        });
                } else if is_from {
                    type_handlers[th_pos].to_jni_converter =
                        Some(ToForeignRetConverter {
                            code: format!("let ret = {}::swig_from(ret, env);",
                                          type_handlers[th_pos].jni_type_name),
                            depends: vec![Rc::new(RefCell::new(Converter::new(item.clone())))],
                        });
                }
            } else {
                panic!("Internal Error: Unknown trait in types map");
            }
        } else {
            panic!("Internal Error: Wrong trait in types map");
        }
    }

    type_handlers
}
