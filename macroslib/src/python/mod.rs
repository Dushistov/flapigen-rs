use proc_macro2::TokenStream;
use quote::quote;
use crate::{
    error::{Result},
    ForeignEnumInfo, ForeignInterface, ForeignerClassInfo, PythonConfig,
    LanguageGenerator, TypeMap,
};

impl LanguageGenerator for PythonConfig {
    fn register_class(
        &self,
        conv_map: &mut TypeMap,
        class: &ForeignerClassInfo
    ) -> Result<()> {
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
        Ok(vec![TokenStream::new()])
    }

    fn generate_interface(
        &self,
        conv_map: &mut TypeMap,
        pointer_target_width: usize,
        interface: &ForeignInterface,
    ) -> Result<Vec<TokenStream>> {
        Ok(vec![TokenStream::new()])
    }

    fn finish(&self, _conv_map: &mut TypeMap) -> Result<Vec<TokenStream>> {
        let registration_code = vec![
            quote!{
                use cpython::py_module_initializer;
                
                py_module_initializer!(librust_swig_test_python, initlibrust_swig_test_python, PyInit_rust_swig_test_python, |py, m| {
                    m.add(py, "__doc__", "This is test module for rust_swig.")?;
                    Ok(())
                });
            }
        ];
        //registration_code.append(&mut self.module_initialization_code.borrow_mut());
        Ok(registration_code)
    }
}
