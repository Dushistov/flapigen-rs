extern crate syntex;
extern crate syntex_syntax;

use syntex::Registry;
use syntex_syntax::ext::base::{ExtCtxt, MacResult};
use syntex_syntax::tokenstream::TokenTree;
use syntex_syntax::codemap::Span;


pub fn register(registry: &mut Registry) {
    registry.add_macro("foreigner_class", expand_foreigner_class);
}

fn expand_foreigner_class<'cx>(cx: &'cx mut ExtCtxt,
                               sp: Span,
                               tokens: &[TokenTree])
                               -> Box<MacResult + 'cx> {
    unimplemented!();
}
