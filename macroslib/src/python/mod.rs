use crate::{
    error::Result, ForeignEnumInfo, ForeignInterface, ForeignerClassInfo, LanguageGenerator,
    PythonConfig, TypeMap,
};
use heck::SnakeCase;
use proc_macro2::TokenStream;
use quote::quote;
use syn::Ident;

impl LanguageGenerator for PythonConfig {
    fn register_class(&self, conv_map: &mut TypeMap, class: &ForeignerClassInfo) -> Result<()> {
        Ok(())
    }

    fn generate(
        &self,
        conv_map: &mut TypeMap,
        _: usize,
        class: &ForeignerClassInfo,
    ) -> Result<Vec<TokenStream>> {
        Ok(vec![TokenStream::new()])
    }

    fn generate_enum(
        &self,
        conv_map: &mut TypeMap,
        pointer_target_width: usize,
        enum_info: &ForeignEnumInfo,
    ) -> Result<Vec<TokenStream>> {
        let enum_name = &enum_info.name;
        let wrapper_mod_name = syn::parse_str::<Ident>(&format!(
            "py_{}",
            enum_info.name.to_string().to_snake_case()
        ))?;
        self.module_initialization_code.borrow_mut().push(quote! {
            {
                m.add_class::<#wrapper_mod_name::#enum_name>(py)?;
            }

        });
        let foreign_variants = enum_info.items.iter().map(|item| &item.name);
        let rust_variants = enum_info.items.iter().map(|item| &item.rust_name);
        let class_code = quote! {
            mod #wrapper_mod_name {
                py_class!(pub class #enum_name |py| {
                    #( static #foreign_variants = super::#rust_variants as u32; )*
                });
            }
        };
        Ok(vec![class_code])
    }

    fn generate_interface(
        &self,
        conv_map: &mut TypeMap,
        pointer_target_width: usize,
        interface: &ForeignInterface,
    ) -> Result<Vec<TokenStream>> {
        Ok(vec![])
    }

    fn init_glue_rs(&self, _conv_map: &mut TypeMap) -> Result<Vec<TokenStream>> {
        Ok(vec![quote! {
            #[macro_use] extern crate cpython;
            use cpython::ObjectProtocol;
        }])
    }

    fn finish_glue_rs(&self, _conv_map: &mut TypeMap) -> Result<Vec<TokenStream>> {
        let module_initialization_code_cell = self.module_initialization_code.borrow();
        let module_initialization_code = &*module_initialization_code_cell;
        let registration_code = vec![quote! {

            py_module_initializer!(librust_swig_test_python, initlibrust_swig_test_python, PyInit_rust_swig_test_python, |py, m| {
                m.add(py, "__doc__", "This is test module for rust_swig.")?;
                #(#module_initialization_code)*
                Ok(())
            });
        }];
        Ok(registration_code)
    }
}
