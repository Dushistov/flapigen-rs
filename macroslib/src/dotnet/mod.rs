mod classes;

use super::*;
// use cpp::{fclass, CppContext};
use error::{ResultDiagnostic, ResultSynDiagnostic, SourceIdSpan};
use file_cache::FileWriteCache;
use itertools::Itertools;
use petgraph::Direction;
use quote::quote;
use rustc_hash::FxHashSet;
use smol_str::SmolStr;
use std::{
    collections::HashMap,
    fs::{self, File},
};
use syn::{parse_str, Ident, Type};
use typemap::{
    ast,
    ty::RustType,
    utils::{self, ForeignMethodSignature, ForeignTypeInfoT},
    ForeignTypeInfo, MapToForeignFlag, FROM_VAR_TEMPLATE, TO_VAR_TEMPLATE,
};
use types::{FnArg, ForeignerClassInfo, ForeignerMethod, MethodVariant, SelfTypeVariant};

enum ArgName {
    SelfArg,
    Named(SmolStr),
    Return,
}

impl ArgName {
    fn rust_variable_name(&self) -> &str {
        match self {
            ArgName::SelfArg => "this",
            ArgName::Return => "ret",
            ArgName::Named(name) => name,
        }
    }

    fn dotnet_variable_name(&self) -> &str {
        match self {
            ArgName::SelfArg => "__this",
            ArgName::Return => "__ret",
            ArgName::Named(name) => name,
        }
    }
}

struct DotNetArgInfo {
    foreign_type: ForeignTypeInfo,
    rust_intermediate_type: RustType,
    dotnet_intermediate_type: String,
    rust_conversion_code: String,
    dotnet_conversion_code: String,
    finalizer: String,
    name: ArgName,
}

impl ForeignTypeInfoT for DotNetArgInfo {
    fn name(&self) -> &str {
        self.foreign_type.name.as_str()
    }
    fn correspoding_rust_type(&self) -> &RustType {
        &self.foreign_type.correspoding_rust_type
    }
}

struct NameGenerator {
    map: HashMap<String, usize>,
}

impl NameGenerator {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    fn new_variant(&mut self, name_base: &str) -> String {
        let i = self.map.entry(name_base.to_owned()).or_insert(0);
        *i += 1;
        format!("{}_{}", name_base, i)
    }

    fn last_variant(&mut self, name_base: &str) -> String {
        let i = self.map.entry(name_base.to_owned()).or_insert(0);
        format!("{}_{}", name_base, i)
    }

    fn first_variant(name_base: &str) -> String {
        format!("{}_{}", name_base, 0)
    }
}

struct DotNetForeignMethodSignature {
    output: DotNetArgInfo,
    input: Vec<DotNetArgInfo>,
    name: String,
    variant: MethodVariant,
    rust_function_call: String,
}

impl ForeignMethodSignature for DotNetForeignMethodSignature {
    type FI = DotNetArgInfo;
    fn output(&self) -> &dyn ForeignTypeInfoT {
        &self.output
    }
    fn input(&self) -> &[DotNetArgInfo] {
        &self.input[..]
    }
}

fn calc_this_type_for_method(_: &TypeMap, class: &ForeignerClassInfo) -> Option<Type> {
    class
        .self_desc
        .as_ref()
        .map(|x| x.constructor_ret_type.clone())
}

pub struct DotNetGenerator<'a> {
    config: &'a DotNetConfig,
    conv_map: &'a mut TypeMap,
    rust_code: Vec<TokenStream>,
    cs_file: FileWriteCache,
}

impl<'a> DotNetGenerator<'a> {
    fn new(config: &'a DotNetConfig, conv_map: &'a mut TypeMap) -> Result<Self> {
        let mut generated_files_registry = FxHashSet::default();
        let cs_file = Self::create_cs_project(config, &mut generated_files_registry)?;

        Ok(Self {
            config,
            conv_map,
            rust_code: Vec::new(),
            cs_file,
        })
    }

    fn generate(mut self, items: Vec<ItemToExpand>) -> Result<Vec<TokenStream>> {
        // let void_type = self.conv_map.find_or_alloc_rust_type_no_src_id(&parse_type!(*mut std::os::raw::c_void));
        // self.conv_map.add_foreign_rust_ty_idx(foreign_name, correspoding_rty)
        // self.conv_map.add_foreign(, foreign_name);

        for item in items {
            match item {
                ItemToExpand::Class(fclass) => {
                    self.generate_class(&fclass)?;
                }
                _ => unimplemented!(), // ItemToExpand::Enum(fenum) => fenum::generate_enum(&mut ctx, &fenum)?,
                                       // ItemToExpand::Interface(finterface) => {
                                       //     finterface::generate_interface(&mut ctx, &finterface)?
                                       // }
            }
        }

        self.finish()?;
        self.cs_file.update_file_if_necessary()?;
        Ok(self.rust_code)
    }

    fn create_cs_project(
        config: &'a DotNetConfig,
        generated_files_registry: &mut FxHashSet<PathBuf>,
    ) -> Result<FileWriteCache> {
        fs::create_dir_all(&config.managed_lib_name).expect("Can't create managed lib directory");

        let mut csproj = File::create(format!("{0}/{0}.csproj", config.managed_lib_name))
            .with_note("Can't create csproj file")?;

        write!(
            csproj,
            r#"
<Project Sdk="Microsoft.NET.Sdk">

<PropertyGroup>
    <TargetFramework>netstandard2.0</TargetFramework>
</PropertyGroup>

</Project>
"#,
        )
        .with_note("Can't write to csproj file")?;

        let cs_file_name = config.managed_lib_name.clone() + ".cs";
        let mut cs_file = FileWriteCache::new(
            PathBuf::from(&config.managed_lib_name).join(cs_file_name),
            generated_files_registry,
        );

        write!(
            cs_file,
            r#"
// Generated by rust_swig. Do not edit.

using System;
using System.Runtime.InteropServices;

namespace {managed_lib_name}
{{

    internal static class RustInterop {{
        [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
        internal static extern void String_delete(IntPtr c_char_ptr);
    }}
"#,
            managed_lib_name = config.managed_lib_name,
            native_lib_name = config.native_lib_name,
        )
        .with_note("Write to memory failed")?;

        Ok(cs_file)
    }

    fn generate_class(&mut self, class: &ForeignerClassInfo) -> Result<()> {
        //self.conv_map.register_foreigner_class(fclass);
        classes::register_class(self.conv_map, class)?;
        self.generate_swig_trait_for_class(class)?;
        self.generate_rust_destructor(class)?;
        self.generate_dotnet_class_code(class)?;

        for method in &class.methods {
            self.generate_method(&class, method)?;
        }

        writeln!(self.cs_file, "}} // class").with_note("Write to memory failed")?;

        Ok(())
    }

    fn class_storage_type(&self, class: &ForeignerClassInfo) -> Option<RustType> {
        Some(
            self.conv_map
                .ty_to_rust_type(&class.self_desc.as_ref()?.constructor_ret_type),
        )
    }

    fn generate_swig_trait_for_class(&mut self, class: &ForeignerClassInfo) -> Result<()> {
        if let Some(this_type) = self.class_storage_type(class) {
            let (_, code_box_this) =
                utils::convert_to_heap_pointer(self.conv_map, &this_type, "this");
            let lifetimes = ast::list_lifetimes(&this_type.ty);
            let unpack_code = utils::unpack_from_heap_pointer(&this_type, TO_VAR_TEMPLATE, true);
            let class_name = &this_type.ty;
            let unpack_code = unpack_code.replace(TO_VAR_TEMPLATE, "p");
            let unpack_code: TokenStream = syn::parse_str(&unpack_code).unwrap_or_else(|err| {
                error::panic_on_syn_error(
                    "internal/c++ foreign class unpack code",
                    unpack_code,
                    err,
                )
            });
            let this_type_ty = this_type.to_type_without_lifetimes();
            let fclass_impl_code: TokenStream = quote! {
                impl<#(#lifetimes),*> SwigForeignClass for #class_name {
                    // fn c_class_name() -> *const ::std::os::raw::c_char {
                    //     swig_c_str!(stringify!(#class_name))
                    // }
                    fn box_object(this: Self) -> *mut ::std::os::raw::c_void {
                        #code_box_this
                        this as *mut ::std::os::raw::c_void
                    }
                    fn unbox_object(p: *mut ::std::os::raw::c_void) -> Self {
                        let p = p as *mut #this_type_ty;
                        #unpack_code
                        p
                    }
                }
            };
            self.rust_code.push(fclass_impl_code);
        }
        Ok(())
    }

    fn generate_rust_destructor(&mut self, class: &ForeignerClassInfo) -> Result<()> {
        // Do not generate destructor for static classes.
        if let Some(_) = class.self_desc {
            let class_name = &class.name;
            let destructor_name = parse_str::<Ident>(&format!("{}_delete", class_name)).unwrap();

            let destructor_code = quote! {
                #[allow(non_snake_case, unused_variables, unused_mut, unused_unsafe)]
                #[no_mangle]
                pub extern "C" fn #destructor_name(this: *mut ::std::os::raw::c_void) {
                    let this = #class_name::unbox_object(this);
                    std::mem::drop(this);
                }
            };
            self.rust_code.push(destructor_code);
        }
        Ok(())
    }

    fn generate_dotnet_class_code(&mut self, class: &ForeignerClassInfo) -> Result<()> {
        let class_name = class.name.to_string();

        if let Some(_) = class.self_desc {
            let rust_destructor_name = class_name.clone() + "_delete";

            write!(
                self.cs_file,
                r#"public class {class_name}: IDisposable {{
        internal IntPtr nativePtr;

        internal {class_name}(IntPtr nativePtr) {{
            this.nativePtr = nativePtr;
        }}

        public void Dispose() {{
            DoDispose();
            GC.SuppressFinalize(this);
        }}

        private void DoDispose() {{
            if (nativePtr != IntPtr.Zero) {{
                {rust_destructor_name}(nativePtr);
                nativePtr = IntPtr.Zero;
            }}
        }}

        [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
        internal static extern void {rust_destructor_name}(IntPtr __this);

        ~{class_name}() {{
            DoDispose();
        }}
"#,
                class_name = class_name,
                rust_destructor_name = rust_destructor_name,
                native_lib_name = self.config.native_lib_name,
            )
            .with_note("Write to memory failed")?;
        } else {
            writeln!(
                self.cs_file,
                "public static class {class_name} {{",
                class_name = class_name,
            )
            .with_note("Write to memory failed")?;
        }

        Ok(())
    }

    fn generate_method(
        &mut self,
        class: &ForeignerClassInfo,
        method: &ForeignerMethod,
    ) -> Result<()> {
        let mut name_generator = NameGenerator::new();
        let foreign_method_signature =
            self.make_foreign_method_signature(class, method, &mut name_generator)?;

        self.write_rust_glue_code(class, &foreign_method_signature)?;
        self.write_pinvoke_function_signature(class, &foreign_method_signature)?;
        self.write_dotnet_wrapper_function(class, &foreign_method_signature, &mut name_generator)?;

        Ok(())
    }

    fn write_rust_glue_code(
        &mut self,
        class: &ForeignerClassInfo,
        foreign_method_signature: &DotNetForeignMethodSignature,
    ) -> Result<()> {
        let method_name = &foreign_method_signature.name;
        let full_method_name = format!("{}_{}", class.name, method_name);

        let convert_input_code = foreign_method_signature
            .input
            .iter()
            .map(|arg| &arg.rust_conversion_code)
            .join("");

        let rust_func_args_str = foreign_method_signature
            .input
            .iter()
            .map(|arg_info| {
                format!(
                    "{}: {}",
                    arg_info.name.rust_variable_name(),
                    arg_info.rust_intermediate_type.typename()
                )
            })
            .join(", ");

        let rust_code_str = format!(
            r#"
    #[allow(non_snake_case, unused_variables, unused_mut, unused_unsafe)]
    #[no_mangle]
    pub extern "C" fn {func_name}({func_args}) -> {return_type} {{
        {convert_input_code}
        let mut {ret_name} = {call};
        {convert_output_code}
        {ret_name}
    }}
"#,
            func_name = full_method_name,
            func_args = rust_func_args_str,
            return_type = foreign_method_signature.output.rust_intermediate_type,
            convert_input_code = convert_input_code,
            ret_name = foreign_method_signature.output.name.rust_variable_name(),
            convert_output_code = foreign_method_signature.output.rust_conversion_code,
            call = foreign_method_signature.rust_function_call,
        );
        self.rust_code
            .push(syn::parse_str(&rust_code_str).with_syn_src_id(class.src_id)?);
        Ok(())
    }

    fn write_pinvoke_function_signature(
        &mut self,
        class: &ForeignerClassInfo,
        foreign_method_signature: &DotNetForeignMethodSignature,
    ) -> Result<()> {
        let method_name = &foreign_method_signature.name;
        let full_method_name = format!("{}_{}", class.name, method_name);
        let pinvoke_args_str = foreign_method_signature
            .input
            .iter()
            .map(|a| {
                format!(
                    "{} {}",
                    a.dotnet_intermediate_type,
                    a.name.dotnet_variable_name()
                )
            })
            .join(", ");
        write!(
            self.cs_file,
            r#"
        //[SuppressUnmanagedCodeSecurity]
        [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
        internal static extern {return_type} {method_name}({args});
"#,
            native_lib_name = self.config.native_lib_name,
            return_type = foreign_method_signature.output.dotnet_intermediate_type,
            method_name = full_method_name,
            args = pinvoke_args_str,
        )
        .with_note("Write to memory failed")?;

        Ok(())
    }

    fn write_dotnet_wrapper_function(
        &mut self,
        class: &ForeignerClassInfo,
        // method: &ForeignerMethod,
        foreign_method_signature: &DotNetForeignMethodSignature,
        name_generator: &mut NameGenerator,
    ) -> Result<()> {
        let maybe_static_str = if foreign_method_signature.variant == MethodVariant::StaticMethod {
            "static"
        } else {
            ""
        };
        let is_constructor = foreign_method_signature.variant == MethodVariant::Constructor;
        let full_method_name = format!("{}_{}", class.name, foreign_method_signature.name);
        let method_name = if is_constructor {
            ""
        } else {
            &foreign_method_signature.name
        };
        let args_to_skip = if let MethodVariant::Method(_) = foreign_method_signature.variant {
            1
        } else {
            0
        };
        let dotnet_args_str = foreign_method_signature
            .input
            .iter()
            .skip(args_to_skip)
            .map(|arg| {
                format!(
                    "{} {}",
                    arg.foreign_type.name,
                    NameGenerator::first_variant(arg.name.dotnet_variable_name())
                )
            })
            .join(", ");

        let this_input_conversion =
            if let MethodVariant::Method(_) = foreign_method_signature.variant {
                "var __this_1 = this.nativePtr;\n"
            } else {
                ""
            };

        let dotnet_input_conversion = this_input_conversion.to_owned()
            + &foreign_method_signature
                .input
                .iter()
                .skip(args_to_skip)
                .map(|arg| &arg.dotnet_conversion_code)
                .join("\n            ");

        let returns_something =
            foreign_method_signature.output.foreign_type.name != "void" && !is_constructor;
        let maybe_return_bind = if returns_something {
            "var __ret_0 = "
        } else if is_constructor {
            "this.nativePtr = "
        } else {
            ""
        };
        let maybe_dotnet_output_conversion = if returns_something {
            &foreign_method_signature.output.dotnet_conversion_code
        } else {
            ""
        };
        let maybe_return = if returns_something {
            format!("return {};", name_generator.last_variant("__ret"))
        } else {
            String::new()
        };

        let finalizers = foreign_method_signature
            .input
            .iter()
            .filter(|arg| !arg.finalizer.is_empty())
            .map(|arg| &arg.finalizer)
            .join("\n            ");

        let pinvoke_call_args = foreign_method_signature
            .input
            .iter()
            .map(|arg| name_generator.last_variant(arg.name.dotnet_variable_name()))
            .join(", ");
        write!(
            self.cs_file,
            r#"
        public {maybe_static} {dotnet_return_type} {method_name}({dotnet_args}) {{
            {dotnet_input_conversion}
            {maybe_return_bind}{full_method_name}({pinvoke_call_args});
            {maybe_dotnet_output_conversion}
            {finalizers}
            {maybe_return}
        }}
"#,
            maybe_static = maybe_static_str,
            dotnet_return_type = foreign_method_signature.output.foreign_type.name,
            method_name = method_name,
            dotnet_args = dotnet_args_str,
            dotnet_input_conversion = dotnet_input_conversion,
            maybe_return_bind = maybe_return_bind,
            full_method_name = full_method_name,
            pinvoke_call_args = pinvoke_call_args,
            maybe_dotnet_output_conversion = maybe_dotnet_output_conversion,
            finalizers = finalizers,
            maybe_return = maybe_return,
        )
        .with_note("Write to memory failed")?;

        Ok(())
    }

    fn make_foreign_method_signature(
        &mut self,
        class: &ForeignerClassInfo,
        method: &ForeignerMethod,
        name_generator: &mut NameGenerator,
    ) -> Result<DotNetForeignMethodSignature> {
        let dummy_ty = parse_type! { () };
        let dummy_rust_ty = self.conv_map.find_or_alloc_rust_type_no_src_id(&dummy_ty);

        let input = method
            .fn_decl
            .inputs
            .iter()
            .map(|arg| match arg {
                FnArg::Default(named_arg) => self.map_type(
                    &named_arg.ty,
                    Direction::Incoming,
                    ArgName::Named(named_arg.name.clone()),
                    name_generator,
                    (class.src_id, named_arg.span),
                ),
                FnArg::SelfArg(span_ref, self_variant) => {
                    let span = *span_ref;
                    // let self_ty = self.class_storage_type(class).ok_or_else(|| {
                    //     DiagnosticError::new(class.src_id, *span, "Non-static methods can only be defined for a class that have a constructor (at least private empty one)")
                    // })?.ty;
                    let self_ty = class.self_desc.as_ref().ok_or_else(|| {
                        DiagnosticError::new(class.src_id, span, "Non-static methods can only be defined for a class that have a constructor (at least private empty one)")
                    })?.self_type.clone();
                    let self_ty_full = match self_variant {
                        SelfTypeVariant::Rptr => parse_type_spanned_checked!(span, & #self_ty),
                        SelfTypeVariant::RptrMut => parse_type_spanned_checked!(span, &mut #self_ty),
                        _ => unimplemented!("Passing self by value not implemented yet"),
                    };
                    // let (self_storage_ty_full, self_method_ty_full): (Type, Type) =
                    // utils::create_suitable_types_for_constructor_and_self(
                    //     self_variant,
                    //     class,
                    //     &self_storage_type.ty,
                    // );
                    self.map_type(
                        &self_ty_full,
                        Direction::Incoming,
                        ArgName::SelfArg,
                        name_generator,
                        (class.src_id, span),
                    )
            }
            })
            .collect::<Result<Vec<_>>>()?;

        let output = match method.fn_decl.output {
            syn::ReturnType::Default => DotNetArgInfo {
                foreign_type: ForeignTypeInfo {
                    name: "void".into(),
                    correspoding_rust_type: dummy_rust_ty.clone(),
                },
                rust_intermediate_type: dummy_rust_ty.clone(),
                name: ArgName::Return,
                rust_conversion_code: String::new(),
                dotnet_conversion_code: String::new(),
                dotnet_intermediate_type: "void".to_owned(),
                finalizer: String::new(),
            },
            syn::ReturnType::Type(_, ref ty) => self.map_type(
                ty,
                Direction::Outgoing,
                ArgName::Return,
                name_generator,
                (class.src_id, class.span()),
            )?,
        };
        Ok(DotNetForeignMethodSignature {
            input,
            output,
            name: method.short_name(),
            variant: method.variant.clone(),
            rust_function_call: method.generate_code_to_call_rust_func(),
        })
    }

    // fn foreign_to_rust_convert_method_inputs(
    //     &mut self,
    //     class: &ForeignerClassInfo,
    //     method: &Foreigner Method,
    //     f_method: &DotNetForeignMethodSignature,
    //     arg_names: &[String],
    //     func_ret_type: &str,
    // ) -> Result<(Vec<TokenStream>, String)> {
    //     let mut code_deps = Vec::new();
    //     let mut ret_code = String::new();

    //     //skip self
    //     let skip_n = match method.variant {
    //         MethodVariant::Method(_) => 1,
    //         _ => 0,
    //     };
    //     for ((to_type, f_from), arg_name) in method
    //         .fn_decl
    //         .inputs
    //         .iter()
    //         .skip(skip_n)
    //         .zip(f_method.input().iter())
    //         .zip(arg_names)
    //     {
    //         let to_named_arg = to_type
    //             .as_named_arg()
    //             .map_err(|err| DiagnosticError::from_syn_err(class.src_id, err))?;
    //         let to: RustType = self.conv_map.find_or_alloc_rust_type(&to_named_arg.ty, class.src_id);
    //         let (mut cur_deps, cur_code) = self.conv_map.convert_rust_types(
    //             f_from.correspoding_rust_type().to_idx(),
    //             to.to_idx(),
    //             arg_name.as_ref(),
    //             arg_name.as_ref(),
    //             func_ret_type,
    //             (class.src_id, to_named_arg.ty.span()),
    //         )?;
    //         code_deps.append(&mut cur_deps);
    //         ret_code.push_str(&cur_code);
    //     }
    //     Ok((code_deps, ret_code))
    // }

    fn map_type(
        &mut self,
        ty: &Type,
        direction: Direction,
        arg_name: ArgName,
        name_generator: &mut NameGenerator,
        span: SourceIdSpan,
    ) -> Result<DotNetArgInfo> {
        let rust_ty = self.conv_map.find_or_alloc_rust_type(ty, span.0);

        let foreign_type_idx = self
            .conv_map
            .map_through_conversation_to_foreign(
                rust_ty.to_idx(),
                direction,
                MapToForeignFlag::FullSearch,
                span,
                calc_this_type_for_method,
            )
            .ok_or_else(|| {
                DiagnosticError::new2(
                    span,
                    format!(
                        "Cannot found a conversion for output type {}",
                        rust_ty.to_string()
                    ),
                )
            })?;
        let foreign_type = &self.conv_map[foreign_type_idx].clone();
        let rule = match direction {
            Direction::Outgoing => foreign_type.into_from_rust.as_ref(),
            Direction::Incoming => foreign_type.from_into_rust.as_ref(),
        }
        .ok_or_else(|| {
            DiagnosticError::new2(
                span,
                format!(
                    "No rule to convert foreign type {} as input/output type",
                    foreign_type.name
                ),
            )
        })?;

        let correspoding_rust_type = self.conv_map[rule.rust_ty].clone();

        if let Some(intermediate) = rule.intermediate.as_ref() {
            // if correspoding_rust_type.typename() == "bool" {
            //     panic!("{:#?}", foreign_type);
            // }
            //            println!("{:#?}", intermediate);
            let intermediate_rust_type = self.conv_map[intermediate.intermediate_ty].clone();
            let intermediate_foreign_type_idx = self
                .conv_map
                .map_through_conversation_to_foreign(
                    intermediate_rust_type.to_idx(),
                    direction,
                    MapToForeignFlag::FastSearch,
                    span,
                    calc_this_type_for_method,
                )
                .ok_or_else(|| {
                    DiagnosticError::new2(
                        span,
                        format!("Can't find intermediate dotnet type for {}", rust_ty),
                    )
                })?;
            let intermediate_foreign_type = self.conv_map[intermediate_foreign_type_idx].clone();

            let (from, to) = match direction {
                Direction::Incoming => (
                    intermediate_rust_type.to_idx(),
                    correspoding_rust_type.to_idx(),
                ),
                Direction::Outgoing => (
                    correspoding_rust_type.to_idx(),
                    intermediate_rust_type.to_idx(),
                    // format!(
                    //     "var {0} = {1}({0});",
                    //     arg_name.dotnet_variable_name(),
                    //     foreign_type.name
                    // ),
                ),
            };
            let (_cur_deps, rust_conversion_code) = self.conv_map.convert_rust_types(
                from,
                to,
                arg_name.rust_variable_name(),
                arg_name.rust_variable_name(),
                "()", // todo
                span,
            )?;

            let dotnet_arg_name = name_generator.last_variant(arg_name.dotnet_variable_name());
            let new_dotnet_arg_name = name_generator.new_variant(arg_name.dotnet_variable_name());

            let dotnet_conversion_code = format!(
                "var {} = {};",
                new_dotnet_arg_name,
                intermediate
                    .conv_code
                    .to_string()
                    .replace(FROM_VAR_TEMPLATE, &dotnet_arg_name)
            );

            let finalizer = intermediate
                .finalizer_code
                .as_ref()
                .map(|str| {
                    str.as_str()
                        .to_owned()
                        .replace(TO_VAR_TEMPLATE, &new_dotnet_arg_name)
                })
                .unwrap_or_default();

            Ok(DotNetArgInfo {
                foreign_type: ForeignTypeInfo {
                    name: foreign_type.typename(),
                    correspoding_rust_type,
                },
                name: arg_name,
                rust_intermediate_type: intermediate_rust_type,
                rust_conversion_code,
                dotnet_intermediate_type: intermediate_foreign_type.name.to_string(),
                dotnet_conversion_code,
                finalizer,
            })
        } else {
            Ok(DotNetArgInfo {
                foreign_type: ForeignTypeInfo {
                    name: foreign_type.typename(),
                    correspoding_rust_type: correspoding_rust_type.clone(),
                },
                name: arg_name,
                rust_intermediate_type: correspoding_rust_type.clone(),
                rust_conversion_code: String::new(),
                dotnet_intermediate_type: foreign_type.typename().to_string(),
                dotnet_conversion_code: String::new(),
                finalizer: String::new(),
            })
        }
    }

    fn finish(&mut self) -> Result<()> {
        writeln!(self.cs_file, "}} // namespace",)?;
        Ok(())
    }
}

impl LanguageGenerator for DotNetConfig {
    fn expand_items(
        &self,
        conv_map: &mut TypeMap,
        _target_pointer_width: usize,
        _code: &[SourceCode],
        items: Vec<ItemToExpand>,
        _remove_not_generated_files: bool,
    ) -> Result<Vec<TokenStream>> {
        DotNetGenerator::new(&self, conv_map)?.generate(items)
    }
}

// struct ObjectTypeInfo {
//     self_type: Option<RustType>,
//     // `self_type` may be wrapped in some smart pointers when stored in memory. That's `storage_type`.
//     // Also referred to as "constructor type". May be the same as `self_type`.
//     storage_type: Option<RustType>,
//     // Conversion from raw C pointer to the storage_type.
//     conversion_code: String,
// }

// impl ObjectTypeInfo {
//     fn new_empty() -> Self {
//         Self {
//             self_type: None,
//             storage_type: None,
//             conversion_code: String::new(),
//         }
//     }

//     fn new(self_type: RustType, storage_type: RustType, conversion_code: String) -> Self {
//         Self {
//             self_type: Some(self_type),
//             storage_type: Some(self_type),
//             conversion_code,
//         }
//     }

//     fn self_type_str(&self) -> String {
//         self.self_type.map(|t| t.to_string()).unwrap_or_default()
//     }

//     fn storage_type_str(&self) -> String {
//         self.storage_type.map(|t| t.to_string()).unwrap_or_default()
//     }
// }
