use crate::{
    error::{DiagnosticError, Result},
    types::ForeignClassInfo,
    MethodVariant, KNOWN_CLASS_DERIVES,
};
use rustc_hash::FxHashMap;

pub(crate) type ClassExtHandlers = FxHashMap<String, Box<dyn Fn(&mut Vec<u8>, &str)>>;
pub struct MethodInfo<'a> {
    pub class_name: &'a str,
    pub method_name: &'a str,
    pub variant: MethodVariant,
}
pub(crate) type MethodExtHandlers = FxHashMap<String, Box<dyn Fn(&mut Vec<u8>, MethodInfo)>>;

pub(crate) fn extend_foreign_class(
    class: &ForeignClassInfo,
    cnt: &mut Vec<u8>,
    class_ext_handlers: &ClassExtHandlers,
    method_ext_handlers: &MethodExtHandlers,
) -> Result<()> {
    for derive in &class.derive_list {
        if KNOWN_CLASS_DERIVES.iter().any(|x| x == derive) {
            continue;
        }
        if let Some(cb) = class_ext_handlers.get(derive) {
            cb(cnt, &class.name.to_string());
        } else {
            return Err(DiagnosticError::new(
                class.src_id,
                class.span(),
                format!(
                    "class {}: has unknown derive attribute {}",
                    class.name, derive
                ),
            ));
        }
    }

    for method in &class.methods {
        for attr in &method.unknown_attrs {
            if let Some(cb) = method_ext_handlers.get(attr) {
                cb(
                    cnt,
                    MethodInfo {
                        class_name: &class.name.to_string(),
                        method_name: &method.short_name(),
                        variant: method.variant,
                    },
                );
            } else {
                return Err(DiagnosticError::new(
                    class.src_id,
                    method.span(),
                    format!(
                        "class {}, method {}: has unknown derive attribute {}",
                        class.name,
                        method.short_name(),
                        attr
                    ),
                ));
            }
        }
    }
    Ok(())
}
