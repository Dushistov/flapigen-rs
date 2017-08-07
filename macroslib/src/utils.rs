use std::{fmt, io};

use syntex_syntax::parse;
use syntex_pos::Span;
use syntex_syntax::ast::FunctionRetTy;
use syntex_syntax::print::pprust;

pub(crate) fn fatal_error(sess: &parse::ParseSess, span: &Span, msg: &str) -> ! {
    sess.span_diagnostic.struct_span_fatal(*span, msg).emit();
    panic!("{}", msg)
}

pub(crate) fn map_write_err(err: io::Error) -> String {
    format!("write failed: {}", err)
}

pub(crate) fn fmt_write_err_map(err: fmt::Error) -> String {
    format!("fmt write error: {}", err)
}

pub(crate) fn function_ret_ty_to_string(x: &FunctionRetTy) -> String {
    use self::FunctionRetTy::*;
    match *x {
        Default(_) => "()".to_string(),
        Ty(ref pty) => pprust::ty_to_string(&*pty),
    }
}
