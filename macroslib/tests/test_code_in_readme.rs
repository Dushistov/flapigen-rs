extern crate rust_swig;
extern crate syntex;
extern crate pulldown_cmark as cmark;
extern crate tempdir;
extern crate env_logger;

use std::path::Path;
use std::fs::File;
use std::io::{Read, Write};
use std::fs;

use tempdir::TempDir;
use cmark::{Event, Parser, Tag};
use rust_swig::{Generator, JavaConfig, LanguageConfig};

#[macro_use]
#[path = "../src/test_helper.rs"]
mod test_helper;

#[test]
fn test_code_in_readme() {
    test_helper::logger_init();
    let tests = parse_readme();
    let tmp_dir = TempDir::new("readme_test").expect("Can not create tmp dir");

    println!("{}: tmp_dir {:?}", file!(), tmp_dir.path());
    for test in &tests {
        if test.text.contains("foreigner_class!") {
            println!("{} with such code:\n{}", test.name, test.text);

            let java_path = tmp_dir.path().join(&test.name).join("java");

            fs::create_dir_all(&java_path).unwrap_or_else(|why| {
                panic!("! {:?}", why.kind());
            });
            let rust_path_src = tmp_dir.path().join(&test.name).join("test.rs.in");
            let mut src = File::create(&rust_path_src).expect("can not create test.rs.in");
            src.write_all(test.text.as_bytes()).unwrap();
            let rust_path_dst = tmp_dir.path().join(&test.name).join("test.rs");

            let mut registry = syntex::Registry::new();
            let swig_gen = Generator::new_with_pointer_target_width(
                LanguageConfig::JavaConfig(JavaConfig::new(java_path, "com.example".into())),
                64,
            );
            swig_gen.register(&mut registry);
            registry
                .expand("rust_swig_test_jni", &rust_path_src, &rust_path_dst)
                .unwrap();
        }
    }
}

struct CodeBlockInfo {
    is_rust: bool,
    should_panic: bool,
    ignore: bool,
    no_run: bool,
    is_old_template: bool,
    template: Option<String>,
}

#[derive(Debug)]
struct Test {
    name: String,
    text: String,
    ignore: bool,
    no_run: bool,
    should_panic: bool,
    template: Option<String>,
}

fn parse_readme() -> Vec<Test> {
    let mut file = File::open(Path::new("../README.md")).expect("Can not open README");
    let mut cnt = String::new();
    file.read_to_string(&mut cnt).unwrap();
    let parser = Parser::new(&cnt);

    let mut test_number = 1;
    let mut code_buffer = None;
    let mut tests = Vec::new();
    for event in parser {
        match event {
            Event::Start(Tag::CodeBlock(ref info)) => {
                let code_block_info = parse_code_block_info(info);
                if code_block_info.is_rust {
                    code_buffer = Some(Vec::new());
                }
            }
            Event::Text(text) => if let Some(ref mut buf) = code_buffer {
                buf.push(text.to_string());
            },
            Event::End(Tag::CodeBlock(ref info)) => {
                let code_block_info = parse_code_block_info(info);
                if let Some(buf) = code_buffer.take() {
                    tests.push(Test {
                        name: format!("test_{}", test_number),
                        text: buf.iter().fold(String::new(), |acc, x| acc + x.as_str()),
                        ignore: code_block_info.ignore,
                        no_run: code_block_info.no_run,
                        should_panic: code_block_info.should_panic,
                        template: code_block_info.template,
                    });
                    test_number += 1;
                }
            }
            _ => (),
        }
    }

    tests
}

fn parse_code_block_info(info: &str) -> CodeBlockInfo {
    // Same as rustdoc
    let tokens = info.split(|c: char| !(c == '_' || c == '-' || c.is_alphanumeric()));

    let mut seen_rust_tags = false;
    let mut seen_other_tags = false;
    let mut info = CodeBlockInfo {
        is_rust: false,
        should_panic: false,
        ignore: false,
        no_run: false,
        is_old_template: false,
        template: None,
    };

    for token in tokens {
        match token {
            "" => {}
            "rust" => {
                info.is_rust = true;
                seen_rust_tags = true
            }
            "should_panic" => {
                info.should_panic = true;
                seen_rust_tags = true
            }
            "ignore" => {
                info.ignore = true;
                seen_rust_tags = true
            }
            "no_run" => {
                info.no_run = true;
                seen_rust_tags = true;
            }
            "skeptic-template" => {
                info.is_old_template = true;
                seen_rust_tags = true
            }
            _ if token.starts_with("skt-") => {
                info.template = Some(token[4..].to_string());
                seen_rust_tags = true;
            }
            _ => seen_other_tags = true,
        }
    }

    info.is_rust &= !seen_other_tags || seen_rust_tags;

    info
}
