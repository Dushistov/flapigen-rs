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
    pub fn add(&mut self, mut err: DiagnosticError) {
        self.data.append(&mut err.data);
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

#[cfg(procmacro2_semver_exempt)]
pub(crate) fn report_parse_error(name: &str, code: &str, main_err: &DiagnosticError) -> ! {
    for err in &main_err.data {
        let span = err.span();
        let start = span.start();
        let end = span.end();

        let mut code_problem = String::new();
        for (i, line) in code.lines().enumerate() {
            if i == start.line {
                code_problem.push_str(if i == end.line {
                    &line[start.column..end.column]
                } else {
                    &line[start.column..]
                });
            } else if i > start.line && i < end.line {
                code_problem.push_str(line);
            } else if i == end.line {
                code_problem.push_str(&line[..end.column]);
                break;
            }
        }

        eprintln!(
            "parsing of {name} failed\nerror: {err} at {line_s}:{col_s} - {line_e}:{col_e} \n{code_problem}",
            name = name, err = err, code_problem = code_problem,
            line_s = start.line, col_s = start.column,
            line_e = end.line, col_e = end.column
        );
    }
    panic!();
}

#[cfg(not(procmacro2_semver_exempt))]
pub(crate) fn report_parse_error(name: &str, _code: &str, err: &DiagnosticError) -> ! {
    panic!("parsing of types map '{}' failed\nerror: '{}'", name, err);
}
