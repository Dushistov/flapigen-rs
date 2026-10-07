
use super::*;
use crate::typemap::ast::DisplayToTokens;
use crate::typemap::MapToForeignFlag;
use crate::{Generator, LanguageConfig};
use petgraph::Direction;
use syn::parse_quote;

#[test]
fn slices_of_arc_and_rc_foreign_objects_are_generated() {
    for smart_pointer in ["Arc", "Rc"] {
        for method in [
            format!("fn Holder::get(&self) -> &[{smart_pointer}<Node>];"),
            format!("fn Holder::take(&self, values: &[{smart_pointer}<Node>]);"),
        ] {
            let output_dir = tempfile::tempdir().unwrap();
            let config = CppConfig::new(output_dir.path().to_path_buf(), "test".into());
            let mut generator =
                Generator::new(LanguageConfig::CppConfig(config)).with_pointer_target_width(64);
            let code = format!(
                "foreign_class!(class Node {{\n\
                     self_type Node;\n\
                     constructor Node::new() -> {smart_pointer}<Node>;\n\
                     }});\n\
                     foreign_class!(class Holder {{\n\
                     self_type Holder;\n\
                     constructor Holder::new() -> Holder;\n\
                     {method}\n\
                     }});"
            );
            let src_id = generator.src_reg.register(SourceCode {
                id_of_code: "arc_rc_slice_without_typemap.rs".into(),
                code,
            });
            generator
                .expand_str(&[src_id], output_dir.path().join("glue.rs"))
                .unwrap_or_else(|error| panic!("failed to generate {method}: {error}"));
            let header =
                std::fs::read_to_string(output_dir.path().join("CRustSliceForeignIndirectNode.h"))
                    .unwrap();
            assert!(header.contains("struct NodeAccess"), "{method}");
            assert!(
                header.contains("CRustSliceForeignIndirectNode_get"),
                "{method}"
            );
        }
    }
}

#[test]
fn plain_class_indirect_slice_explains_missing_borrowed_wrapper() {
    let output_dir = tempfile::tempdir().unwrap();
    let config = CppConfig::new(output_dir.path().to_path_buf(), "test".into());
    let mut generator =
        Generator::new(LanguageConfig::CppConfig(config)).with_pointer_target_width(64);
    let src_id = generator.src_reg.register(SourceCode {
        id_of_code: "plain_indirect_slice.rs".into(),
        code: "foreign_class!(#[derive(PlainClass)] class Node {
                self_type Node;
                constructor Node::new() -> Rc<Node>;
            });
            foreign_class!(class Holder {
                self_type Holder;
                constructor Holder::new() -> Holder;
                fn Holder::get(&self) -> &[Rc<Node>];
            });"
        .into(),
    });
    let error = generator
        .expand_str(&[src_id], output_dir.path().join("glue.rs"))
        .expect_err("PlainClass cannot produce borrowed slice elements");
    assert!(
        error.to_string().contains("PlainClass")
            && error.to_string().contains("borrowed C++ wrapper"),
        "{error}"
    );
}

#[test]
fn indirect_foreign_class_slices_are_not_direct_mapped() {
    for (constructor_type, slice_type) in [
        ("Arc<Node>", "Node"),
        ("Rc<Node>", "Node"),
        ("Box<Box<Node>>", "Node"),
        ("Node", "Arc<Node>"),
        ("Node", "Rc<Node>"),
        ("Node", "Box<Node>"),
        ("Box<Box<Node>>", "Box<Node>"),
    ] {
        let output_dir = tempfile::tempdir().unwrap();
        let config = CppConfig::new(output_dir.path().to_path_buf(), "test".into());
        let mut generator =
            Generator::new(LanguageConfig::CppConfig(config)).with_pointer_target_width(64);
        let code = format!(
            "foreign_class!(class Node {{\n\
                 self_type Node;\n\
                 constructor Node::new() -> {constructor_type};\n\
                 }});\n\
                 foreign_class!(class Holder {{\n\
                 self_type Holder;\n\
                 constructor Holder::new() -> Holder;\n\
                 fn Holder::slice(&self) -> &[{slice_type}];\n\
                 }});"
        );
        let src_id = generator.src_reg.register(SourceCode {
            id_of_code: "indirect_slice.rs".into(),
            code,
        });
        let error = generator
            .expand_str(&[src_id], output_dir.path().join("glue.rs"))
            .expect_err("indirect slice must not use direct storage");
        assert!(
            error.to_string().contains("conversion"),
            "unexpected error for constructor {constructor_type} and &[{slice_type}]: {error}"
        );
    }
}

#[test]
fn unsupported_foreign_class_vectors_do_not_use_inline_access() {
    for (constructor_type, element_type) in [
        ("Box<Box<Node>>", "Box<Box<Node>>"),
        ("Arc<Node>", "Node"),
        ("Rc<Node>", "Node"),
        ("Node", "Arc<Node>"),
    ] {
        let output_dir = tempfile::tempdir().unwrap();
        let config = CppConfig::new(output_dir.path().to_path_buf(), "test".into());
        let mut generator =
            Generator::new(LanguageConfig::CppConfig(config)).with_pointer_target_width(64);
        let code = format!(
            "foreign_class!(class Node {{
                    self_type Node;
                    constructor Node::new() -> {constructor_type};
                }});
                foreign_class!(class Holder {{
                    self_type Holder;
                    constructor Holder::new() -> Holder;
                    fn Holder::values(&self) -> Vec<{element_type}>;
                }});"
        );
        let src_id = generator.src_reg.register(SourceCode {
            id_of_code: "unsupported_foreign_vec.rs".into(),
            code,
        });
        let error = generator
            .expand_str(&[src_id], output_dir.path().join("glue.rs"))
            .expect_err("unsupported vector must not use the inline class policy");
        assert!(
            error.to_string().contains("conversion"),
            "unexpected error for Vec<{element_type}>: {error}"
        );
    }
}

#[test]
fn plain_class_vector_explains_missing_borrowed_wrapper() {
    let output_dir = tempfile::tempdir().unwrap();
    let config = CppConfig::new(output_dir.path().to_path_buf(), "test".into());
    let mut generator =
        Generator::new(LanguageConfig::CppConfig(config)).with_pointer_target_width(64);
    let src_id = generator.src_reg.register(SourceCode {
        id_of_code: "plain_foreign_vec.rs".into(),
        code: "foreign_class!(#[derive(PlainClass)] class Node {
                self_type Node;
                constructor Node::new() -> Arc<Node>;
            });
            foreign_class!(class Holder {
                self_type Holder;
                constructor Holder::new() -> Holder;
                fn Holder::values(&self) -> Vec<Arc<Node>>;
            });"
        .into(),
    });
    let error = generator
        .expand_str(&[src_id], output_dir.path().join("glue.rs"))
        .expect_err("PlainClass cannot produce borrowed vector elements");
    assert!(
        error.to_string().contains("PlainClass") && error.to_string().contains("borrowed wrapper"),
        "{error}"
    );
}

#[test]
fn nested_foreign_vectors_reject_non_direct_classes() {
    for (class_attribute, constructor_type, element_type) in [
        ("#[derive(PlainClass)]", "Node", "Node"),
        ("", "Arc<Node>", "Arc<Node>"),
        ("", "Rc<Node>", "Rc<Node>"),
    ] {
        let output_dir = tempfile::tempdir().unwrap();
        let config = CppConfig::new(output_dir.path().to_path_buf(), "test".into());
        let mut generator =
            Generator::new(LanguageConfig::CppConfig(config)).with_pointer_target_width(64);
        let code = format!(
            "foreign_class!({class_attribute} class Node {{
                    self_type Node;
                    constructor Node::new() -> {constructor_type};
                }});
                foreign_class!(class Holder {{
                    self_type Holder;
                    constructor Holder::new() -> Holder;
                    fn Holder::values() -> Vec<Vec<{element_type}>>;
                }});"
        );
        let src_id = generator.src_reg.register(SourceCode {
            id_of_code: "unsupported_nested_foreign_vec.rs".into(),
            code,
        });
        let error = generator
            .expand_str(&[src_id], output_dir.path().join("glue.rs"))
            .expect_err("unsupported nested foreign vector should fail");
        assert!(
            error
                .to_string()
                .contains("nested foreign-class vectors require"),
            "unexpected error for Vec<Vec<{element_type}>>: {error}"
        );
    }
}

#[test]
fn slice_conversion_graph_keeps_element_types_separate() {
    let output_dir = tempfile::tempdir().unwrap();
    let config = CppConfig::new(output_dir.path().to_path_buf(), "test".into());
    let mut generator =
        Generator::new(LanguageConfig::CppConfig(config)).with_pointer_target_width(64);
    let src_id = generator.src_reg.register(SourceCode {
        id_of_code: "slice_conversion_graph.rs".into(),
        code: r#"
foreign_class!(class Foo {
    self_type Foo;
    constructor Foo::new() -> Foo;
});
foreign_class!(class Bar {
    self_type Bar;
    constructor Bar::new() -> Bar;
});
foreign_class!(class Slices {
    self_type Slices;
    constructor Slices::new() -> Slices;
    fn Slices::foo_out(&self) -> &[Foo];
    fn Slices::bar_out(&self) -> &[Bar];
    fn Slices::foo_in(&self, value: &[Foo]);
    fn Slices::bar_in(&self, value: &[Bar]);
    fn Slices::u32_out(&self) -> &[u32];
    fn Slices::u64_out(&self) -> &[u64];
    fn Slices::u32_in(&self, value: &[u32]);
    fn Slices::u64_in(&self, value: &[u64]);
});
"#
        .into(),
    });
    generator
        .expand_str(&[src_id], output_dir.path().join("glue.rs"))
        .unwrap();

    // Check that the fixture really instantiated both directions for
    // every slice type before testing for an unintended cross-type path.
    for slice in [
        parse_quote!(&[Foo]),
        parse_quote!(&[Bar]),
        parse_quote!(&[u32]),
        parse_quote!(&[u64]),
    ] {
        let slice_idx = generator
            .conv_map
            .find_or_alloc_rust_type(&slice, SourceId::none())
            .to_idx();
        for direction in [Direction::Outgoing, Direction::Incoming] {
            let foreign_idx = generator
                .conv_map
                .map_through_conversion_to_foreign(
                    slice_idx,
                    direction,
                    MapToForeignFlag::FastSearch,
                    invalid_src_id_span(),
                    |_, _| None,
                )
                .unwrap_or_else(|| {
                    panic!(
                        "missing {direction:?} mapping for {}",
                        DisplayToTokens(&slice)
                    )
                });
            let abi_idx = match direction {
                Direction::Outgoing => {
                    generator.conv_map[foreign_idx]
                        .into_from_rust
                        .as_ref()
                        .unwrap()
                        .rust_ty
                }
                Direction::Incoming => {
                    generator.conv_map[foreign_idx]
                        .from_into_rust
                        .as_ref()
                        .unwrap()
                        .rust_ty
                }
            };
            let (from_idx, to_idx) = match direction {
                Direction::Outgoing => (slice_idx, abi_idx),
                Direction::Incoming => (abi_idx, slice_idx),
            };
            assert!(
                    generator
                        .conv_map
                        .convert_rust_types(
                            from_idx,
                            to_idx,
                            "from",
                            "to",
                            "()",
                            invalid_src_id_span(),
                        )
                        .is_ok(),
                    "missing {direction:?} conversion for {} through {}",
                    DisplayToTokens(&slice),
                    generator.conv_map[abi_idx],
                );
        }
    }

    // A shared CRustSlice intermediate would connect each pair here,
    // even though the element types differ.
    for (from, to) in [
        (parse_quote!(&[Foo]), parse_quote!(&[Bar])),
        (parse_quote!(&[Bar]), parse_quote!(&[Foo])),
        (parse_quote!(&[u32]), parse_quote!(&[u64])),
        (parse_quote!(&[u64]), parse_quote!(&[u32])),
    ] {
        let from_idx = generator
            .conv_map
            .find_or_alloc_rust_type(&from, SourceId::none())
            .to_idx();
        let to_idx = generator
            .conv_map
            .find_or_alloc_rust_type(&to, SourceId::none())
            .to_idx();
        assert!(
            generator
                .conv_map
                .convert_rust_types(from_idx, to_idx, "from", "to", "()", invalid_src_id_span(),)
                .is_err(),
            "unexpected conversion from {} to {}",
            DisplayToTokens(&from),
            DisplayToTokens(&to),
        );
    }
}
