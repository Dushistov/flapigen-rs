use std::collections::HashMap;
use std::fmt::Write;

use syntex_syntax::parse::{token};
use syntex_syntax::{ast};
use syntex_syntax::ptr::P;
use syntex_syntax::print::pprust;

#[derive(PartialEq)]
pub enum FuncVariant {
    Constructor, Method, StaticMethod
}

impl FuncVariant {
    pub fn from_str(ident: &token::InternedString) -> Option<FuncVariant> {
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
    pub name_alias: Option<token::InternedString>,
    pub may_return_error: bool,
}

pub struct TypeHandler {
    pub rust_type_name: &'static str,
    pub jni_type_name: &'static str,
    pub java_type_name: &'static str,
    pub from_jni_converter: Option<fn(&str)->String>,
    pub to_jni_converter: Option<fn() -> String>,
}

pub type RustToJavaTypes<'a> = HashMap<&'static str, &'a TypeHandler>;

pub fn get_type_handler<'a, 'b>(types_map: &RustToJavaTypes<'a>, name: &'b str) -> &'a TypeHandler {
    types_map.get(name).expect(&format!("Unknown type `{}`", name))
}

impl ForeignerMethod {
    pub fn args(&self, use_comma_if_need: bool) -> String {
        let skip_n =  if self.func_type == FuncVariant::Method { 1 } else { 0 };
        let mut res = String::new();
        if use_comma_if_need && skip_n < self.in_out_type.inputs.len() {
            res.push_str(", ");
        }
        for (i, _) in self.in_out_type.inputs.iter().skip(skip_n).enumerate() {
            if i == (self.in_out_type.inputs.len() - 1 - skip_n) {
                write!(&mut res, "a_{}", i)
            } else {
                write!(&mut res, "a_{}, ", i)
            }
            .unwrap();
        }
        res
    }

    pub fn args_with_java_types(&self, use_comma_if_need: bool, types_map: &RustToJavaTypes) -> String {
        let skip_n =  if self.func_type == FuncVariant::Method { 1 } else { 0 };
        let mut res = String::new();
        if use_comma_if_need && skip_n < self.in_out_type.inputs.len() {
            write!(&mut res, ", ").unwrap();
        }
        for (i, item_arg) in self.in_out_type.inputs.iter().skip(skip_n).enumerate() {
            let type_name = get_type_handler(types_map, pprust::ty_to_string(&*item_arg.ty).as_str()).java_type_name;
            if i == (self.in_out_type.inputs.len() - 1 - skip_n) {
                write!(&mut res, "{} a_{}", type_name, i)
            } else {
                write!(&mut res, "{} a_{}, ", type_name, i)
            }
            .unwrap();
        }
        res
    }

    pub fn java_return_type(&self, types_map: &RustToJavaTypes) -> &'static str {
        match &self.in_out_type.output {
            &ast::FunctionRetTy::Default(_) => "void",
            &ast::FunctionRetTy::Ty(ref ret_type) =>
                get_type_handler(types_map, pprust::ty_to_string(&*ret_type).as_str()).java_type_name
        }
    }

    pub fn short_name(&self) -> token::InternedString {
        if let Some(ref name) = self.name_alias {
            name.clone()
        } else {
            match self.path.segments.len() {
                0 => token::InternedString::new(""),
                n => self.path.segments[n - 1].identifier.name.as_str()
            }
        }
    }

    pub fn full_name(&self) -> String {
        format!("{}", self.path)
    }
}
