use std::collections::HashMap;
use std::fmt::Write;
use std::cell::RefCell;
use std::rc::Rc;

use syntex_syntax::ast;
use syntex_syntax::ptr::P;
use syntex_syntax::print::pprust;
use syntex_syntax::symbol::{InternedString, Symbol};

use deref_type_name;

#[derive(PartialEq)]
pub enum FuncVariant {
    Constructor,
    Method,
    StaticMethod,
}

impl FuncVariant {
    pub fn from_str(ident: &InternedString) -> Option<FuncVariant> {
        match &**ident {
            "constructor" => Some(FuncVariant::Constructor),
            "method" => Some(FuncVariant::Method),
            "static_method" => Some(FuncVariant::StaticMethod),
            _ => None,
        }
    }
}

pub struct ForeignerMethod {
    pub func_type: FuncVariant,
    pub path: ast::Path,
    pub in_out_type: P<ast::FnDecl>,
    pub name_alias: Option<InternedString>,
    pub may_return_error: bool,
    pub private: bool,
}

pub struct Converter {
    pub used: bool,
    pub item: Option<P<ast::Item>>,
}

impl Converter {
    pub fn new(item: P<ast::Item>) -> Converter {
        Converter {
            used: false,
            item: Some(item),
        }
    }
}

#[derive(Clone)]
pub struct FromForeignArgConverter {
    pub code: String,
    pub depends: Vec<Rc<RefCell<Converter>>>,
}

#[allow(non_snake_case)]
pub fn FromForeignArgConverter(code: String) -> FromForeignArgConverter {
    FromForeignArgConverter {
        code: code,
        depends: Vec::new(),
    }
}

impl FromForeignArgConverter {
    pub fn apply(&self, arg_name: &str) -> String {
        for dep in &self.depends {
            dep.borrow_mut().used = true;
        }
        self.code.replace("{arg_name}", arg_name)
    }
}

#[derive(Clone)]
pub struct ToForeignRetConverter {
    pub code: String,
    pub depends: Vec<Rc<RefCell<Converter>>>,
}

#[allow(non_snake_case)]
pub fn ToForeignRetConverter(code: String) -> ToForeignRetConverter {
    ToForeignRetConverter {
        code: code,
        depends: Vec::new(),
    }
}

impl ToForeignRetConverter {
    pub fn apply(&self) -> &str {
        for dep in &self.depends {
            dep.borrow_mut().used = true;
        }
        &self.code
    }
}

#[derive(Clone)]
pub struct TypeHandler {
    pub rust_type_name: String,
    pub jni_type_name: String,
    pub java_type_name: String,
    pub from_jni_converter: Option<FromForeignArgConverter>,
    pub to_jni_converter: Option<ToForeignRetConverter>,
}

pub struct ForeignerClassInfo<'a> {
    pub package_name: String,
    pub class_name: &'a str,
    pub methods: Vec<ForeignerMethod>,
    pub self_rust_type: ast::Path,
    pub constructor_ret_type: Option<ast::Ty>,
    pub this_type_for_method: Option<ast::Ty>,
    pub foreigner_code: String,
}

pub fn full_java_class_name(package_name: &str, class_name: &str) -> String {
    let mut ret: String = package_name.into();
    ret.push('.');
    ret.push_str(class_name);
    ret.replace(".", "/")
}

impl<'a> ForeignerClassInfo<'a> {
    pub fn full_java_class_name(&self) -> String {
        full_java_class_name(&self.package_name, self.class_name)
    }
}


impl<'a, 'b> Into<TypeHandler> for &'a ForeignerClassInfo<'b> {
    fn into(self) -> TypeHandler {
        TypeHandler {
            rust_type_name: pprust::ty_to_string(self.this_type_for_method.as_ref().unwrap()),
            jni_type_name: "jobject".into(),
            java_type_name: self.class_name.into(),
            from_jni_converter: None,
            to_jni_converter: None,
        }
    }
}

pub type RustToJavaTypes<'a> = HashMap<&'a str, &'a TypeHandler>;

fn compare_types_skip_lifetime(a: &str, b: &str) -> bool {
    deref_type_name(a) == deref_type_name(b)
}

pub fn get_type_handler<'a, 'b>(types_map: &RustToJavaTypes<'a>, name: &'b str) -> &'a TypeHandler {
    
    if let Some(v) = types_map.get(name) {
        return v;
    } else if name.starts_with('&') {
        for (k, v) in types_map.iter() {
            if k.starts_with('&') && compare_types_skip_lifetime(k, name) {
                return v;
            }
        }
    }
    panic!("Unknown type `{}`", name);
}

impl ForeignerMethod {
    pub fn args(&self, use_comma_if_need: bool) -> String {
        let skip_n = if self.func_type == FuncVariant::Method {
            1
        } else {
            0
        };
        let mut res = String::new();
        if use_comma_if_need && skip_n < self.in_out_type.inputs.len() {
            res.push_str(", ");
        }
        for (i, _) in self.in_out_type
                .inputs
                .iter()
                .skip(skip_n)
                .enumerate() {
            if i == (self.in_out_type.inputs.len() - 1 - skip_n) {
                    write!(&mut res, "a_{}", i)
                } else {
                    write!(&mut res, "a_{}, ", i)
                }
                .unwrap();
        }
        res
    }
    
    pub fn short_name(&self) -> InternedString {
        if let Some(ref name) = self.name_alias {
            name.clone()
        } else {
            match self.path.segments.len() {
                0 => Symbol::gensym("").as_str(),
                n => self.path.segments[n - 1].identifier.name.as_str(),
            }
        }
    }

    pub fn full_name(&self) -> String {
        format!("{}", self.path)
    }
}
