use syntex_errors::DiagnosticBuilder;
use syntex_pos::Span;
use syntex_syntax::parse::ParseSess;

pub(crate) fn fatal_error<'a>(sess: &'a ParseSess, sp: Span, msg: &str) -> DiagnosticBuilder<'a> {
    sess.span_diagnostic.struct_span_fatal(sp, msg)
}
