use rustc_hash::FxHashMap;
use syn::{
    parse::{Parse, ParseStream},
    parse_quote,
    spanned::Spanned,
    visit::Visit,
    LitStr, Macro, Token,
};

#[derive(Default)]
pub struct JniCacheMacroCalls {
    pub calls: FxHashMap<String, JniFindClass>,
}

impl JniCacheMacroCalls {
    pub fn global_vars(&self) -> Vec<syn::Item> {
        let mut ret = Vec::with_capacity(self.calls.len());
        for find_class in self.calls.values() {
            let id = &find_class.id;
            ret.push(parse_quote! {
                static mut #id: jclass = ::std::ptr::null_mut();
            });
            for sub_id in &find_class.methods {
                let id = &sub_id.id;
                ret.push(parse_quote! {
                    static mut #id: jmethodID = ::std::ptr::null_mut();
                });
            }
            for sub_id in &find_class.static_methods {
                let id = &sub_id.id;
                ret.push(parse_quote! {
                    static mut #id: jmethodID = ::std::ptr::null_mut();
                });
            }
            for sub_id in &find_class.static_fields {
                let id = &sub_id.id;
                ret.push(parse_quote! {
                    static mut #id: jfieldID = ::std::ptr::null_mut();
                });
            }
            for sub_id in &find_class.fields {
                let id = &sub_id.id;
                ret.push(parse_quote! {
                    static mut #id: jfieldID = ::std::ptr::null_mut();
                });
            }
        }
        ret
    }
}

#[derive(PartialEq, Debug)]
pub struct JniFindClass {
    pub id: syn::Ident,
    pub path: LitStr,
    pub methods: Vec<JniClassItemWithId>,
    pub static_methods: Vec<JniClassItemWithId>,
    pub static_fields: Vec<JniClassItemWithId>,
    pub fields: Vec<JniClassItemWithId>,
}
impl Parse for JniFindClass {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let id = input.parse()?;
        input.parse::<Token![,]>()?;
        let path = input.parse()?;
        Ok(Self {
            id,
            path,
            methods: Vec::new(),
            static_methods: Vec::new(),
            static_fields: Vec::new(),
            fields: Vec::new(),
        })
    }
}

#[derive(PartialEq, Debug)]
pub struct JniClassItemWithId {
    pub id: syn::Ident,
    pub class_id: syn::Ident,
    pub name: LitStr,
    pub sig: LitStr,
}
impl Parse for JniClassItemWithId {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let id = input.parse()?;
        input.parse::<Token![,]>()?;
        let class_id = input.parse()?;
        input.parse::<Token![,]>()?;
        let name = input.parse()?;
        input.parse::<Token![,]>()?;
        let sig = input.parse()?;
        if input.peek(Token![,]) {
            input.parse::<Token![,]>()?;
        }
        Ok(Self {
            id,
            class_id,
            name,
            sig,
        })
    }
}

macro_rules! add_jni_method_id {
    ($mac:ident, $calls: ident, $sub_calls: ident, $name: expr) => {
        let get_method_id: JniClassItemWithId =
            syn::parse2($mac.tokens.clone()).unwrap_or_else(|err| {
                panic!(
                    "Can not parse '{}' call: {}, code: {}",
                    $name, err, $mac.tokens
                )
            });
        let class_id = get_method_id.class_id.to_string();
        let find_class = $calls.get_mut(&class_id).unwrap_or_else(|| {
            panic!(
                "Can not find class_id for {}({}, {}, ...) call",
                $name, get_method_id.id, class_id
            );
        });
        if let Some(wrong_usage_pos) = find_class.$sub_calls.iter().position(|elem| {
            elem.name == get_method_id.name
                && elem.sig == get_method_id.sig
                && get_method_id.id != elem.id
        }) {
            let prev_get_method_id = &find_class.$sub_calls[wrong_usage_pos];
            panic!(
                "{} called twice with different id, {} vs {} for class {}",
                $name, prev_get_method_id.id, get_method_id.id, class_id
            );
        }
        if !find_class.$sub_calls.iter().any(|x| *x == get_method_id) {
            find_class.$sub_calls.push(get_method_id);
        }
    };
}

pub struct JniCacheMacroCallsVisitor<'a> {
    pub inner: &'a mut JniCacheMacroCalls,
    pub errors: Vec<syn::Error>,
}

impl<'ast> Visit<'ast> for JniCacheMacroCallsVisitor<'ast> {
    fn visit_macro(&mut self, mac: &'ast Macro) {
        static SWIG_JNI_GET_METHOD_ID: &str = "swig_jni_get_method_id";
        static SWIG_JNI_GET_STATIC_METHOD_ID: &str = "swig_jni_get_static_method_id";
        static SWIG_JNI_GET_STATIC_FIELD_ID: &str = "swig_jni_get_static_field_id";
        static SWIG_JNI_GET_FIELD_ID: &str = "swig_jni_get_field_id";

        if mac.path.is_ident("swig_jni_find_class") {
            let find_class: JniFindClass =
                syn::parse2(mac.tokens.clone()).expect("Can not parse swig_jni_find_class call");
            let id = find_class.id.to_string();
            if let Some(call) = self.inner.calls.get(&id) {
                if *call != find_class {
                    println!(
                        "waring=You use the same id '{}' for different classes '{}' vs '{}'",
                        id,
                        call.path.value(),
                        find_class.path.value()
                    );
                    self.errors.push(syn::Error::new(
                        mac.span(),
                        format!(
                            "You use the same id '{}' for different classes '{}' vs '{}'",
                            id,
                            call.path.value(),
                            find_class.path.value()
                        ),
                    ));
                    return;
                }
            }
            self.inner.calls.insert(id, find_class);
        } else if mac.path.is_ident(SWIG_JNI_GET_METHOD_ID) {
            let calls = &mut self.inner.calls;
            add_jni_method_id!(mac, calls, methods, SWIG_JNI_GET_METHOD_ID);
        } else if mac.path.is_ident(SWIG_JNI_GET_STATIC_METHOD_ID) {
            let calls = &mut self.inner.calls;
            add_jni_method_id!(mac, calls, static_methods, SWIG_JNI_GET_STATIC_METHOD_ID);
        } else if mac.path.is_ident(SWIG_JNI_GET_STATIC_FIELD_ID) {
            let calls = &mut self.inner.calls;
            add_jni_method_id!(mac, calls, static_fields, SWIG_JNI_GET_STATIC_FIELD_ID);
        } else if mac.path.is_ident(SWIG_JNI_GET_FIELD_ID) {
            let calls = &mut self.inner.calls;
            add_jni_method_id!(mac, calls, fields, SWIG_JNI_GET_FIELD_ID);
        }
        syn::visit::visit_macro(self, mac)
    }
}
