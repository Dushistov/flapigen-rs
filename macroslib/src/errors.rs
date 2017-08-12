use syntex_syntax::parse::ParseSess;
use syntex_pos::Span;
use syntex_errors::DiagnosticBuilder;

pub(crate) fn fatal_error<'a>(sess: &'a ParseSess, sp: Span, msg: &str) -> DiagnosticBuilder<'a> {
    sess.span_diagnostic.struct_span_fatal(sp, msg)
}
