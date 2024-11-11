mod fclass;
mod fenum;
mod find_cache;
mod finterface;
mod java_code;
mod map_class_self_type;
mod map_type;
mod rust_code;

use log::debug;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use rustc_hash::{FxHashMap, FxHashSet};
use smol_str::SmolStr;
use std::{fmt, io::Write, path::PathBuf};
use syn::{spanned::Spanned, Type};

use crate::{
    error::{invalid_src_id_span, DiagnosticError, Result},
    extension::{ClassExtHandlers, ExtHandlers, MethodExtHandlers},
    file_cache::FileWriteCache,
    typemap::{
        ast::{
            check_if_smart_pointer_return_inner_type, if_result_return_ok_err_types,
            if_ty_result_return_ok_type, DisplayToTokens, UniqueName,
        },
        ty::RustType,
        utils::{
            configure_ftype_rule, remove_files_if, validate_cfg_options, ForeignMethodSignature,
            ForeignTypeInfoT,
        },
        ForeignTypeInfo, TypeMapConvRuleInfo,
    },
    types::{ForeignClassInfo, ForeignMethod, ItemToExpand, MethodVariant},
    JavaConfig, JavaReachabilityFence, LanguageGenerator, SourceCode, TypeMap,
    SMART_PTR_COPY_TRAIT, WRITE_TO_MEM_FAILED_MSG,
};
use map_class_self_type::register_typemap_for_self_type;

const INTERNAL_PTR_MARKER: &str = "InternalPointerMarker";
const JAVA_RUST_SELF_NAME: &str = "mNativeObj";
const REACHABILITY_FENCE_CLASS: &str = "JNIReachabilityFence";

struct JavaContext<'a> {
    cfg: &'a JavaConfig,
    conv_map: &'a mut TypeMap,
    pointer_target_width: usize,
    rust_code: &'a mut Vec<TokenStream>,
    generated_foreign_files: &'a mut FxHashSet<PathBuf>,
    java_type_to_jni_sig_map: FxHashMap<SmolStr, SmolStr>,
    class_ext_handlers: &'a ClassExtHandlers,
    method_ext_handlers: &'a MethodExtHandlers,
}

#[derive(Clone, Copy, Debug)]
enum NullAnnotation {
    NonNull,
    Nullable,
}

#[derive(Debug)]
struct JavaForeignTypeInfo {
    pub base: ForeignTypeInfo,
    pub java_converter: Option<JavaConverter>,
    annotation: Option<NullAnnotation>,
}

impl ForeignTypeInfoT for JavaForeignTypeInfo {
    fn corresponding_rust_type(&self) -> &RustType {
        &self.base.corresponding_rust_type
    }
}

#[derive(Debug)]
struct JavaConverter {
    java_transition_type: UniqueName,
    annotation: Option<NullAnnotation>,
    converter: String,
}

impl AsRef<ForeignTypeInfo> for JavaForeignTypeInfo {
    fn as_ref(&self) -> &ForeignTypeInfo {
        &self.base
    }
}

impl From<ForeignTypeInfo> for JavaForeignTypeInfo {
    fn from(x: ForeignTypeInfo) -> Self {
        JavaForeignTypeInfo {
            base: ForeignTypeInfo {
                name: x.name,
                corresponding_rust_type: x.corresponding_rust_type,
            },
            java_converter: None,
            annotation: None,
        }
    }
}

struct JniForeignMethodSignature {
    output: JavaForeignTypeInfo,
    input: Vec<JavaForeignTypeInfo>,
}

impl ForeignMethodSignature for JniForeignMethodSignature {
    type FI = JavaForeignTypeInfo;
    fn input(&self) -> &[JavaForeignTypeInfo] {
        &self.input[..]
    }
}

impl JavaConfig {
    fn register_class(&self, ctx: &mut JavaContext, class: &ForeignClassInfo) -> Result<()> {
        class
            .validate_class()
            .map_err(|err| DiagnosticError::new(class.src_id, class.span(), err))?;
        if let Some(self_desc) = class.self_desc.as_ref() {
            let constructor_ret_type = &self_desc.constructor_ret_type;
            let this_type_for_method = if_ty_result_return_ok_type(constructor_ret_type)
                .unwrap_or_else(|| constructor_ret_type.clone());

            let mut traits = vec!["SwigForeignClass"];
            if class.clone_derived() {
                traits.push("Clone");
            }
            if class.copy_derived() {
                if !class.clone_derived() {
                    traits.push("Clone");
                }
                traits.push("Copy");
            }
            if class.smart_ptr_copy_derived() {
                traits.push(SMART_PTR_COPY_TRAIT);
            }

            let this_type: RustType = ctx.conv_map.find_or_alloc_rust_type_that_implements(
                &this_type_for_method,
                &traits,
                class.src_id,
            );
            if class.smart_ptr_copy_derived() {
                if class.copy_derived() {
                    println!(
                        "cargo:warning=class {} marked as Copy and {}, ignore Copy",
                        class.name, SMART_PTR_COPY_TRAIT
                    );
                }
                if check_if_smart_pointer_return_inner_type(&this_type, "Rc").is_none()
                    && check_if_smart_pointer_return_inner_type(&this_type, "Arc").is_none()
                {
                    return Err(DiagnosticError::new(
                        class.src_id,
                        this_type.ty.span(),
                        format!(
                            "class {} marked as {}, but type '{}' is not Arc<> or Rc<>",
                            class.name, SMART_PTR_COPY_TRAIT, this_type
                        ),
                    ));
                }
            }
            register_typemap_for_self_type(ctx, class, this_type, self_desc)?;
        }

        let _ = ctx
            .conv_map
            .find_or_alloc_rust_type(&class.self_type_as_ty(), class.src_id);

        Ok(())
    }
}

impl LanguageGenerator for JavaConfig {
    fn expand_items(
        &self,
        conv_map: &mut TypeMap,
        pointer_target_width: usize,
        code: &[SourceCode],
        items: Vec<ItemToExpand>,
        remove_not_generated_files: bool,
        ext_handlers: ExtHandlers,
    ) -> Result<Vec<TokenStream>> {
        let mut ret = Vec::with_capacity(items.len());
        let mut generated_foreign_files = FxHashSet::default();
        let mut ctx = JavaContext {
            cfg: self,
            conv_map,
            pointer_target_width,
            rust_code: &mut ret,
            generated_foreign_files: &mut generated_foreign_files,
            java_type_to_jni_sig_map: rust_code::predefined_java_type_to_jni_sig(),
            class_ext_handlers: ext_handlers.class_ext_handlers,
            method_ext_handlers: ext_handlers.method_ext_handlers,
        };
        init(&mut ctx, code)?;
        for item in &items {
            if let ItemToExpand::Class(ref fclass) = item {
                self.register_class(&mut ctx, fclass)?;
            }
        }
        for item in items {
            match item {
                ItemToExpand::Class(fclass) => {
                    fclass::generate(&mut ctx, &fclass)?;
                }
                ItemToExpand::Enum(fenum) => {
                    fenum::generate_enum(&mut ctx, &fenum)?;
                }
                ItemToExpand::Interface(finterface) => {
                    finterface::generate_interface(&mut ctx, &finterface)?;
                }
            }
        }

        if remove_not_generated_files {
            remove_files_if(&self.output_dir, |path| {
                if let Some(ext) = path.extension() {
                    if ext == "java" && !generated_foreign_files.contains(path) {
                        return true;
                    }
                }
                false
            })
            .map_err(DiagnosticError::map_any_err_to_our_err)?;
        }

        Ok(ret)
    }
    fn post_proccess_code(
        &self,
        _conv_map: &mut TypeMap,
        _pointer_target_width: usize,
        mut generated_code: Vec<u8>,
    ) -> Result<Vec<u8>> {
        rust_code::generate_load_unload_jni_funcs(&mut generated_code)?;
        Ok(generated_code)
    }
}

fn method_name(method: &ForeignMethod, f_method: &JniForeignMethodSignature) -> String {
    let need_conv = f_method.input.iter().any(|v: &JavaForeignTypeInfo| {
        v.java_converter
            .as_ref()
            .map(|x| !x.converter.is_empty())
            .unwrap_or(false)
    }) || f_method
        .output
        .java_converter
        .as_ref()
        .map(|x| !x.converter.is_empty())
        .unwrap_or(false);
    match method.variant {
        MethodVariant::StaticMethod if !need_conv => method.short_name().as_str().to_string(),
        MethodVariant::Method(_) | MethodVariant::StaticMethod => {
            format!("do_{}", method.short_name())
        }
        MethodVariant::Constructor => "init".into(),
    }
}

fn java_class_full_name(package_name: &str, class_name: &str) -> String {
    let mut ret: String = package_name.into();
    ret.push('.');
    ret.push_str(class_name);
    ret
}

fn java_class_name_to_jni(full_name: &str) -> String {
    full_name.replace('.', "/")
}

fn calc_this_type_for_method(tm: &TypeMap, class: &ForeignClassInfo) -> Option<Type> {
    class
        .self_desc
        .as_ref()
        .map(|x| &x.constructor_ret_type)
        .map(|constructor_ret_type| {
            Some(
                if_result_return_ok_err_types(
                    &tm.ty_to_rust_type_checked(constructor_ret_type)
                        .unwrap_or_else(|| {
                            panic!(
                                "Internal error: constructor type {} for class {} unknown",
                                DisplayToTokens(constructor_ret_type),
                                class.name
                            );
                        }),
                )
                .map(|(ok_ty, _err_ty)| ok_ty)
                .unwrap_or_else(|| constructor_ret_type.clone()),
            )
        })
        .unwrap_or(None)
}

fn merge_rule(ctx: &mut JavaContext, mut rule: TypeMapConvRuleInfo) -> Result<()> {
    debug!("merge_rule begin {:?}", rule);
    if rule.is_empty() {
        return Err(DiagnosticError::new(
            rule.src_id,
            rule.span,
            format!("rule {:?} is empty", rule),
        ));
    }
    let all_options = {
        let mut opts = FxHashSet::<&'static str>::default();
        opts.insert("NullAnnotations");
        opts.insert("NoNullAnnotations");
        opts
    };
    validate_cfg_options(&rule, &all_options)?;
    let options = {
        let mut opts = FxHashSet::<&'static str>::default();
        if ctx.cfg.null_annotation_package.is_some() {
            opts.insert("NullAnnotations");
        } else {
            opts.insert("NoNullAnnotations");
        }
        opts
    };
    if rule.c_types.is_some() {
        return Err(DiagnosticError::new(
            rule.src_id,
            rule.span,
            "c_types not supported for Java/JNI",
        ));
    }
    if !rule.f_code.is_empty() {
        unimplemented!();
    }
    configure_ftype_rule(&mut rule.ftype_left_to_right, "=>", rule.src_id, &options)?;
    configure_ftype_rule(&mut rule.ftype_right_to_left, "<=", rule.src_id, &options)?;
    ctx.conv_map.merge_conv_rule(rule.src_id, rule)?;
    Ok(())
}

fn init(ctx: &mut JavaContext, _code: &[SourceCode]) -> Result<()> {
    if !(ctx.cfg.output_dir.exists() && ctx.cfg.output_dir.is_dir()) {
        return Err(DiagnosticError::map_any_err_to_our_err(format!(
            "Path {} not exists or not directory",
            ctx.cfg.output_dir.display()
        )));
    }
    ctx.conv_map
        .find_or_alloc_rust_type_no_src_id(&parse_type! { jint });
    ctx.conv_map
        .find_or_alloc_rust_type_no_src_id(&parse_type! { jlong });
    let dummy_rust_ty = ctx
        .conv_map
        .find_or_alloc_rust_type_no_src_id(&parse_type! { () });

    let not_merged_data = ctx.conv_map.take_not_merged_not_generic_rules();
    for rule in not_merged_data {
        merge_rule(ctx, rule)?;
    }
    let src_path = ctx
        .cfg
        .output_dir
        .join(format!("{INTERNAL_PTR_MARKER}.java"));
    let mut src_file = FileWriteCache::new(&src_path, ctx.generated_foreign_files);
    writeln!(
        src_file,
        r#"
// Automatically generated by flapigen
package {package};

/*package*/ enum {enum_name} {{
    RAW_PTR;
}}"#,
        package = ctx.cfg.package_name,
        enum_name = INTERNAL_PTR_MARKER,
    )
    .expect(WRITE_TO_MEM_FAILED_MSG);
    src_file.update_file_if_necessary().map_err(|err| {
        DiagnosticError::new2(
            invalid_src_id_span(),
            format!("write to {} failed: {}", src_path.display(), err),
        )
    })?;
    match ctx.cfg.reachability_fence {
        JavaReachabilityFence::Std => {}
        JavaReachabilityFence::GenerateFence(max_args) => {
            let src_path = ctx
                .cfg
                .output_dir
                .join(format!("{REACHABILITY_FENCE_CLASS}.java"));
            let mut src_file = FileWriteCache::new(&src_path, ctx.generated_foreign_files);
            write!(
                src_file,
                r#"
// Automatically generated by flapigen
package {package};

/*package*/ final class {class_name} {{
    private {class_name}() {{}}"#,
                package = ctx.cfg.package_name,
                class_name = REACHABILITY_FENCE_CLASS,
            )
            .expect(WRITE_TO_MEM_FAILED_MSG);

            let mut f_method = JniForeignMethodSignature {
                output: JavaForeignTypeInfo {
                    base: ForeignTypeInfo {
                        name: "void".into(),
                        corresponding_rust_type: dummy_rust_ty.clone(),
                    },
                    java_converter: None,
                    annotation: None,
                },
                input: vec![],
            };

            let mut jni_args = Vec::with_capacity(max_args);

            for i in 1..=max_args {
                let java_method_name = format!("reachabilityFence{}", i);
                write!(
                    src_file,
                    "\n    /*package*/ static native void {}(Object ref1",
                    java_method_name
                )
                .expect(WRITE_TO_MEM_FAILED_MSG);
                for j in 2..=i {
                    write!(src_file, ", Object ref{}", j).expect(WRITE_TO_MEM_FAILED_MSG);
                }
                src_file.write_all(b");").expect(WRITE_TO_MEM_FAILED_MSG);

                f_method.input.push(JavaForeignTypeInfo {
                    base: ForeignTypeInfo {
                        name: "Object".into(),
                        corresponding_rust_type: dummy_rust_ty.clone(),
                    },
                    java_converter: None,
                    annotation: None,
                });
                let jni_func_name = rust_code::generate_jni_func_name(
                    ctx,
                    REACHABILITY_FENCE_CLASS,
                    invalid_src_id_span(),
                    &java_method_name,
                    MethodVariant::StaticMethod,
                    &f_method,
                    false,
                )?;
                let jni_func_name = syn::Ident::new(&jni_func_name, Span::call_site());
                jni_args.push(quote!(_: jobject));
                let jni_args = &jni_args;
                ctx.rust_code.push(quote! {
                    #[allow(unused_variables, unused_mut, non_snake_case, unused_unsafe)]
                    #[no_mangle]
                    pub extern "C" fn #jni_func_name(_env: *mut JNIEnv, _: jclass, #(#jni_args),*) {
                    }
                });
            }
            src_file.write_all(b"}\n").expect(WRITE_TO_MEM_FAILED_MSG);

            src_file.update_file_if_necessary().map_err(|err| {
                DiagnosticError::new2(
                    invalid_src_id_span(),
                    format!("write to {} failed: {}", src_path.display(), err),
                )
            })?;
        }
    }
    Ok(())
}

fn map_write_err<Err: fmt::Display>(err: Err) -> String {
    format!("write failed: {}", err)
}
