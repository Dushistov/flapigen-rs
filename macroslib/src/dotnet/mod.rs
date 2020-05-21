use super::*;
// use cpp::{fclass, CppContext};
use error::SourceIdSpan;
use file_cache::FileWriteCache;
use petgraph::Direction;
use rustc_hash::FxHashSet;
use std::fs::{self, File};
use syn::Type;
use typemap::{
    utils::{foreign_to_rust_convert_method_inputs, ForeignMethodSignature, ForeignTypeInfoT, foreign_from_rust_convert_method_output},
    ForeignTypeInfo, MapToForeignFlag,
};
use types::{FnArg, ForeignerClassInfo, ForeignerMethod, MethodVariant};
use itertools::Itertools;

struct DotNetForeignMethodSignature {
    output: ForeignTypeInfo,
    input: Vec<ForeignTypeInfo>,
}

impl ForeignMethodSignature for DotNetForeignMethodSignature {
    type FI = ForeignTypeInfo;
    fn output(&self) -> &dyn ForeignTypeInfoT {
        &self.output
    }
    fn input(&self) -> &[ForeignTypeInfo] {
        &self.input[..]
    }
}

fn calc_this_type_for_method(_: &TypeMap, class: &ForeignerClassInfo) -> Option<Type> {
    class
        .self_desc
        .as_ref()
        .map(|x| x.constructor_ret_type.clone())
}

fn make_foreign_method_signature(
    conv_map: &mut TypeMap,
    class: &ForeignerClassInfo,
    method: &ForeignerMethod,
) -> Result<DotNetForeignMethodSignature> {
    let dummy_ty = parse_type! { () };
    let dummy_rust_ty = conv_map.find_or_alloc_rust_type_no_src_id(&dummy_ty);

    let input = method
        .fn_decl
        .inputs
        .iter()
        .map(|arg| match arg {
            FnArg::Default(named_arg) => map_type(
                conv_map,
                &named_arg.ty,
                Direction::Incoming,
                (class.src_id, named_arg.span),
            ),
            FnArg::SelfArg(_, _) => {
                unimplemented!("Methods not supported yet");
            }
        })
        .collect::<Result<Vec<_>>>()?;

    let output = match method.fn_decl.output {
        syn::ReturnType::Default => ForeignTypeInfo {
            name: "void".into(),
            correspoding_rust_type: dummy_rust_ty.clone(),
        },
        syn::ReturnType::Type(_, ref ty) => map_type(
            conv_map,
            ty,
            Direction::Outgoing,
            (class.src_id, class.span()),
        )?,
    };
    Ok(DotNetForeignMethodSignature { input, output })
}

fn map_type(
    conv_map: &mut TypeMap,
    ty: &Type,
    direction: Direction,
    span: SourceIdSpan,
) -> Result<ForeignTypeInfo> {
    let rust_ty = conv_map.find_or_alloc_rust_type(ty, span.0);

    let foreign_type_idx = conv_map
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
    let foreign_type = &conv_map[foreign_type_idx];
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
    if let Some(_intermediate) = rule.intermediate.as_ref() {
        unimplemented!("Intemediate type not supported yet");
    }

    Ok(ForeignTypeInfo {
        name: foreign_type.typename(),
        correspoding_rust_type: conv_map[rule.rust_ty].clone(),
    })
}

fn generate_method(
    conv_map: &mut TypeMap,
    config: &DotNetConfig,
    fclass: &ForeignerClassInfo,
    method: &ForeignerMethod,
    out_rust_code: &mut Vec<TokenStream>,
    out_pinvoke_file: &mut FileWriteCache,
) -> Result<()> {
    if method.variant == MethodVariant::StaticMethod {
        let method_name = method.short_name();
        let ret_name = "ret_generated_0";
        let foreign_method_signature = make_foreign_method_signature(conv_map, fclass, method)?;
        let rust_return_type = foreign_method_signature
            .output
            .correspoding_rust_type
            .typename();
        let (_deps_code_in, convert_input_code) = foreign_to_rust_convert_method_inputs(
            conv_map,
            fclass.src_id,
            method,
            &foreign_method_signature,
            method.arg_names_without_self(),
            rust_return_type
        )?;
        let (_deps_code_out, convert_output_code) = foreign_from_rust_convert_method_output(
            conv_map,
            fclass.src_id,
            &method.fn_decl.output,
            foreign_method_signature.output.correspoding_rust_type.to_idx(),
            ret_name,
            &rust_return_type,
        )?;

        let rust_func_args_str = method.arg_names_without_self().zip(foreign_method_signature.input.iter()).map(|(name, foreign_type)|{
            format!("{}: {}", name, foreign_type.correspoding_rust_type().typename())
        }).join(", ");

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
            func_name = method_name,
            func_args = rust_func_args_str,
            return_type = rust_return_type,
            convert_input_code = convert_input_code,
            ret_name = ret_name,
            convert_output_code = convert_output_code,
            call = method.generate_code_to_call_rust_func(),
        );
        out_rust_code.push(
            syn::parse_str(&rust_code_str)
                .map_err(|err| DiagnosticError::from_syn_err(fclass.src_id, err))?,
        );

        let pinvoke_args_str = method.arg_names_without_self().zip(foreign_method_signature.input.iter()).map(|(name, foreign_type)|{
            format!("{} {}", foreign_type.name, name)
        }).join(", ");

        let pinvoke_return_type = foreign_method_signature.output.name();

        write!(
            out_pinvoke_file,
            r#"

        //[SuppressUnmanagedCodeSecurity]
        [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
        public static extern {return_type} {method_name}({args});

"#,
            native_lib_name = config.native_lib_name,
            return_type = pinvoke_return_type,
            method_name = method_name,
            args = pinvoke_args_str,
        )
        .expect("Write to memory failed");
    } else {
        unimplemented!()
    }
    Ok(())
}

// pub struct DotNetGenerator<'a> {
//     conv_map: &'a mut TypeMap,
//     rust_code: Vec<TokenStream>,
//     pinvoke_file: FileWriteCache,
//     // config: DotNetConfig,
// }

// impl<'a> DotNetGenerator<'a> {
//     fn new(config: &DotNetConfig)
// }

impl LanguageGenerator for DotNetConfig {
    fn expand_items(
        &self,
        conv_map: &mut TypeMap,
        _target_pointer_width: usize,
        _code: &[SourceCode],
        items: Vec<ItemToExpand>,
        _remove_not_generated_files: bool,
    ) -> Result<Vec<TokenStream>> {
        let mut rust_code = Vec::<TokenStream>::new();
        // let mut files = FxHashMap::<SmolStr, FileWriteCache>::default();
        let mut generated_foreign_files = FxHashSet::default();
        fs::create_dir_all(&self.managed_lib_name).expect("Can't create managed lib directory");
        for item in items {
            match item {
                ItemToExpand::Class(fclass) => {
                    let class_name = fclass.name.to_string();
                    let pinvoke_file_name = class_name.clone() + "PI.cs";
                    let mut pinvoke_file = FileWriteCache::new(
                        PathBuf::from(&self.managed_lib_name).join(pinvoke_file_name),
                        &mut generated_foreign_files,
                    );

                    write!(
                        pinvoke_file,
                        r#"
using System;
using System.Runtime.InteropServices;

namespace {managed_lib_name}
{{
    public static class {class_name}
    {{

"#,
                        managed_lib_name = self.managed_lib_name,
                        class_name = class_name,
                    )
                    .expect("Write to memory failed");

                    for method in &fclass.methods {
                        generate_method(
                            conv_map,
                            self,
                            &fclass,
                            method,
                            &mut rust_code,
                            &mut pinvoke_file,
                        )?;
                    }
                    write!(
                        pinvoke_file,
                        r#"
    }}
}}
"#,
                    )
                    .expect("Write to memory failed");
                    pinvoke_file
                        .update_file_if_necessary()
                        .map_err(|err| DiagnosticError::new(fclass.src_id, fclass.span(), err))?;
                }
                _ => unimplemented!(), // ItemToExpand::Enum(fenum) => fenum::generate_enum(&mut ctx, &fenum)?,
                                       // ItemToExpand::Interface(finterface) => {
                                       //     finterface::generate_interface(&mut ctx, &finterface)?
                                       // }
            }
            let mut csproj = File::create(format!("{0}/{0}.csproj", self.managed_lib_name))
                .expect("Can't create csproj file");
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
            .expect("Can't write to csproj file");
        }
        Ok(rust_code)
    }
}
