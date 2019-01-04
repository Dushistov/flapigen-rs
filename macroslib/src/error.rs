use std::fmt::Display;

use proc_macro2::Span;

#[derive(Debug)]
pub(crate) struct DiagnosticError {
    data: Vec<syn::Error>,
}

impl DiagnosticError {
    pub fn new<T: Display>(sp: Span, err: T) -> Self {
        DiagnosticError {
            data: vec![syn::Error::new(sp, err)],
        }
    }
    pub fn span_note<T: Display>(&mut self, sp: Span, err: T) {
        self.data.push(syn::Error::new(sp, err));
    }
}

impl Display for DiagnosticError {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::result::Result<(), core::fmt::Error> {
        for x in &self.data {
            write!(f, "{}", x)?;
        }
        Ok(())
    }
}

impl From<syn::Error> for DiagnosticError {
    fn from(x: syn::Error) -> DiagnosticError {
        DiagnosticError { data: vec![x] }
    }
}

pub(crate) type Result<T> = std::result::Result<T, DiagnosticError>;
