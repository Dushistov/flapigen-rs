use std::fmt::{Display, Write};

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
    pub fn register_src_if_no(&mut self, src_id: String, src: String) {
        if self.src_id.is_empty() {
            self.src_id = src_id;
            self.src = src;
        }
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
        let nlines = end.line - start.line + 1;
        for (i, line) in main_err
            .src
            .lines()
            .skip(start.line - 1)
            .take(nlines)
            .enumerate()
        {
            code_problem.push_str(&line);
            code_problem.push('\n');
            if i == 0 && start.column > 0 {
                write!(&mut code_problem, "{:1$}", ' ', start.column)
                    .expect("write to String failed");
            }
            let code_problem_len = if i == 0 {
                if i == nlines - 1 {
                    end.column - start.column
                } else {
                    line.len() - start.column - 1
                }
            } else if i != nlines - 1 {
                line.len()
            } else {
                end.column
            };
            write!(&mut code_problem, "{:^^1$}\n", '^', code_problem_len)
                .expect("write to String failed");
            if i == end.line {
                break;
            }
        }

        eprintln!(
            "parsing of {name} failed\nerror: {err}\n{code_problem}\nAt {name}:{line_s}:{col_s}",
            name = main_err.src_id,
            err = err,
            code_problem = code_problem,
            line_s = start.line,
            col_s = start.column,
        );
    }
    panic!();
}
