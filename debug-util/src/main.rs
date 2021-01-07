use std::{env, fs, io::Read, path::Path};

use flapigen::{CppConfig, Generator, JavaConfig, LanguageConfig};

fn main() {
    env_logger::init();

    let type_map = if let Some(type_map_pos) = env::args().position(|x| x == "--type-map") {
        Some(
            env::args()
                .nth(type_map_pos + 1)
                .expect("No path after --type-map"),
        )
    } else {
        None
    };
    let use_cpp_lang = env::args().any(|x| x == "--cpp-lang");
    let in_path = env::args().last().expect("No path to file to preprocess");
    let in_path = Path::new(&in_path);

    let cur_dir = env::current_dir().expect("Can not get current directory");
    let out_path = cur_dir.join(if use_cpp_lang { "cpp" } else { "java" });
    fs::create_dir_all(&out_path).unwrap_or_else(|why| {
        panic!("! {:?}", why.kind());
    });
    let swig_gen = Generator::new(if !use_cpp_lang {
        LanguageConfig::JavaConfig(JavaConfig::new(out_path.clone(), "com.example".into()))
    } else {
        LanguageConfig::CppConfig(CppConfig::new(out_path.clone(), "core".into()))
    })
    .with_pointer_target_width(64);

    let swig_gen = if let Some(type_map) = type_map {
        let mut tm_f = fs::File::open(&type_map)
            .unwrap_or_else(|err| panic!("Can not open {:?}: {}", type_map, err));
        let mut type_map = String::new();
        tm_f.read_to_string(&mut type_map).unwrap();

        swig_gen.merge_type_map("type_map", &type_map)
    } else {
        swig_gen
    };

    swig_gen.expand("test", in_path, out_path.join("out.rs"));
}
