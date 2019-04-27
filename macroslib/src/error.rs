use std::fmt::Display;

use proc_macro2::Span;

#[derive(Debug)]
pub(crate) struct DiagnosticError {
    src_id: String,
    src: String,
    data: Vec<syn::Error>,
}

impl DiagnosticError {
    pub fn new<T: Display>(sp: Span, err: T) -> Self {
        DiagnosticError {
            src_id: String::new(),
            src: String::new(),
            data: vec![syn::Error::new(sp, err)],
        }
    }
    pub fn span_note<T: Display>(&mut self, sp: Span, err: T) {
        self.data.push(syn::Error::new(sp, err));
    }
    pub fn register_src(&mut self, src_id: String, src: String) {
        self.src_id = src_id;
        self.src = src;
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
        DiagnosticError {
            src_id: String::new(),
            src: String::new(),
            data: vec![x],
        }
    }
}

pub(crate) type Result<T> = std::result::Result<T, DiagnosticError>;

pub(crate) fn panic_on_parse_error(main_err: &DiagnosticError) -> ! {
    eprintln!("error in {}", main_err.src_id);
    for err in &main_err.data {
        let span = err.span();
        let start = span.start();
        let end = span.end();

        let mut code_problem = String::new();
        for (i, line) in main_err.src.lines().enumerate() {
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
            name = main_err.src_id, err = err, code_problem = code_problem,
            line_s = start.line, col_s = start.column,
            line_e = end.line, col_e = end.column
        );
    }
    panic!();
}
