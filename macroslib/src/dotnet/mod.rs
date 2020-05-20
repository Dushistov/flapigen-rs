use super::*;
// use cpp::{fclass, CppContext};
use error::panic_on_syn_error;
use file_cache::FileWriteCache;
use rustc_hash::{FxHashMap, FxHashSet};
use smol_str::SmolStr;
use types::MethodVariant;
use std::fs::{self, File};

// struct DotNetContext {
//     common_files: &'a mut FxHashMap<SmolStr, FileWriteCache>,
//     generated_foreign_files: &'a mut FxHashSet<PathBuf>,
// }

impl LanguageGenerator for DotNetConfig {
    fn expand_items(
        &self,
        conv_map: &mut TypeMap,
        target_pointer_width: usize,
        code: &[SourceCode],
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

                    write!(pinvoke_file, r#"
using System;
using System.Runtime.InteropServices;

namespace {managed_lib_name}
{{
    public static class {class_name}
    {{

"#,
                    managed_lib_name = self.managed_lib_name,
                    class_name = class_name,
                ).expect("Write to memory failed");

                    for method in &fclass.methods {
                        if method.variant == MethodVariant::StaticMethod {
                            let method_name = method.short_name();
                            let rust_code_str = format!(
                                r#"
                        #[allow(non_snake_case, unused_variables, unused_mut, unused_unsafe)]
                        #[no_mangle]
                        pub extern "C" fn {func_name}() {{
                            {call};
                        }}
                        "#,
                                func_name = method_name,
                                call = method.generate_code_to_call_rust_func(),
                            );
                            rust_code.push(syn::parse_str(&rust_code_str).map_err(|err| {
                                DiagnosticError::from_syn_err(fclass.src_id, err)
                            })?);

                            write!(pinvoke_file, r#"

        //[SuppressUnmanagedCodeSecurity]
        [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
        public static extern void {method_name}();

"#,
                                native_lib_name = self.native_lib_name,
                                method_name = method_name,                            
                            ).expect("Write to memory failed");
                        } else {
                            unimplemented!()
                        }
                    }
                    write!(pinvoke_file, r#"
    }}
}}
"#,
                ).expect("Write to memory failed");
                    pinvoke_file.update_file_if_necessary().map_err(|err| DiagnosticError::new(fclass.src_id, fclass.span(), err))?;
                }
                _ => unimplemented!(), // ItemToExpand::Enum(fenum) => fenum::generate_enum(&mut ctx, &fenum)?,
                                       // ItemToExpand::Interface(finterface) => {
                                       //     finterface::generate_interface(&mut ctx, &finterface)?
                                       // }
            }
            let mut csproj = File::create(format!("{0}/{0}.csproj", self.managed_lib_name)).expect("Can't create csproj file");
            write!(csproj, r#"
<Project Sdk="Microsoft.NET.Sdk">

    <PropertyGroup>
        <TargetFramework>netstandard2.0</TargetFramework>
    </PropertyGroup>
          
</Project>          
"#,
            ).expect("Can't write to csproj file");
        }
        Ok(rust_code)
    }
}
