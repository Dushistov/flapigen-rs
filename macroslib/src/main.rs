//extern crate env_logger;

extern crate rust_swig;
extern crate syntex;

use std::path::Path;
use std::{env, fs};
use rust_swig::{Generator, JavaConfig, LanguageConfig};
use syntex::Registry;
use std::io::Read;

fn main() {
    let type_map = if let Some("--type-map") = env::args().nth(1).as_ref().map(String::as_str) {
        Some(env::args().nth(2).unwrap())
    } else {
        None
    };
    let in_path = env::args().last().expect("No path to file to preprocess");
    let in_path = Path::new(&in_path);
    //  env_logger::init().unwrap();

    let cur_dir = env::current_dir().expect("Can not get current directory");
    let java_path = cur_dir.join("java");
    fs::create_dir_all(&java_path).unwrap_or_else(|why| {
        panic!("! {:?}", why.kind());
    });
    let mut registry = Registry::new();
    let swig_gen = Generator::new_with_pointer_target_width(
        LanguageConfig::JavaConfig(JavaConfig::new(java_path, "com.example".into())),
        64,
    );
    let swig_gen = if let Some(type_map) = type_map {
        let mut tm_f = fs::File::open(&type_map)
            .unwrap_or_else(|err| panic!("Can not open {:?}: {}", type_map, err));
        let mut type_map = String::new();
        tm_f.read_to_string(&mut type_map).unwrap();

        swig_gen.merge_type_map("type_map", &type_map)
    } else {
        swig_gen
    };
    swig_gen.register(&mut registry);

    let mut in_f =
        fs::File::open(in_path).unwrap_or_else(|err| panic!("Can not open {:?}: {}", in_path, err));
    let mut code = String::new();
    in_f.read_to_string(&mut code).unwrap();

    let res_code = registry
        .expand_str("test", in_path.to_str().unwrap(), &code)
        .unwrap();
    println!("{}", res_code);
}
