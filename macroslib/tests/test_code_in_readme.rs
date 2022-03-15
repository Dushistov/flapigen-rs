use std::{
    fs,
    fs::File,
    io::{Read, Write},
    path::Path,
};

use flapigen::{CppConfig, Generator, JavaConfig, LanguageConfig};
use pulldown_cmark::{CodeBlockKind, Event, Parser, Tag};
use tempfile::tempdir;

#[test]
fn test_code_in_readme() {
    let _ = env_logger::try_init();
    let tests = parse_readme();
    let tmp_dir = tempdir().expect("Can not create tmp dir");

    println!("{}: tmp_dir {}", file!(), tmp_dir.path().display());
    for test in &tests {
        if test.in_rust
            && (test.text.contains("foreigner_class!") || test.text.contains("foreign_class"))
        {
            println!("{} with such code:\n{}", test.name, test.text);
            fs::create_dir_all(&tmp_dir.path().join(&test.name)).unwrap();
            let rust_path_src = tmp_dir.path().join(&test.name).join("test.rs.in");
            let mut src = File::create(&rust_path_src).expect("can not create test.rs.in");
            src.write_all(test.text.as_bytes()).unwrap();
            let rust_path_dst = tmp_dir.path().join(&test.name).join("test.rs");

            {
                let java_path = tmp_dir.path().join(&test.name).join("java");

                fs::create_dir_all(&java_path).unwrap();

                let swig_gen = Generator::new(LanguageConfig::JavaConfig(JavaConfig::new(
                    java_path,
                    "com.example".into(),
                )))
                .with_pointer_target_width(64);
                swig_gen.expand("flapigen_test_jni", &rust_path_src, &rust_path_dst);
            }

            {
                let cpp_path = tmp_dir.path().join(&test.name).join("c++");

                fs::create_dir_all(&cpp_path).unwrap();
                let swig_gen = Generator::new(LanguageConfig::CppConfig(CppConfig::new(
                    cpp_path,
                    "com_example".into(),
                )))
                .with_pointer_target_width(64);
                swig_gen.expand("flapigen_test_c++", &rust_path_src, &rust_path_dst);
            }
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

struct Code {
    name: String,
    text: String,
    in_rust: bool,
}

fn parse_readme() -> Vec<Code> {
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
            Event::Text(text) => {
                if let Some(ref mut buf) = code_buffer {
                    buf.push(text.to_string());
                }
            }
            Event::End(Tag::CodeBlock(ref info)) => {
                let code_block_info = parse_code_block_info(info);
                if let Some(buf) = code_buffer.take() {
                    tests.push(Code {
                        name: format!("test_{}", test_number),
                        text: buf.iter().fold(String::new(), |acc, x| acc + x.as_str()),
                        in_rust: code_block_info.is_rust,
                    });
                    test_number += 1;
                }
            }
            _ => (),
        }
    }

    tests
}

fn parse_code_block_info(info: &CodeBlockKind) -> CodeBlockInfo {
    // Same as rustdoc
    let info: &str = match info {
        CodeBlockKind::Indented => "",
        CodeBlockKind::Fenced(x) => x.as_ref(),
    };
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
