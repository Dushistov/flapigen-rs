use std::collections::HashMap;

lazy_static! {
    static ref JAVA_TYPE_NAMES_FOR_JNI_SIGNATURE: HashMap<String, &'static str> = {
        let mut m = HashMap::new();
        m.insert("String".into(), "Ljava.lang.String;");
        m.insert("int".into(), "I");
        m
    };
}

pub fn generate_func_name<'a, IterType: ::std::iter::Iterator<Item=&'a String>>(package_name: &str, class_name: &str, func_name: &str, overloaded: bool, args_types_iter: IterType) -> String
{
    let mut output = String::new();
    output.push_str("Java_");
    fn escape_underscore(input: &str, mut output: &mut String) {
        for c in input.chars() {
            match c {
                '.' => output.push('_'),
                '[' => output.push_str("_3"),
                '_' => output.push_str("_1"),
                ';' => output.push_str("_2"),
                _ => output.push(c),
            }
        }
    }
    escape_underscore(package_name, &mut output);
    output.push_str("_");
    escape_underscore(class_name, &mut output);
    output.push_str("_");
    escape_underscore(func_name, &mut output);

    if overloaded {
        output.push_str("__");
        for it in args_types_iter {
            escape_underscore(JAVA_TYPE_NAMES_FOR_JNI_SIGNATURE.get(&*it)
                              .expect(&format!("jni gen func name: Unknown Java type `{}`", *it)),
                              &mut output);
        }
    }

    output
}
