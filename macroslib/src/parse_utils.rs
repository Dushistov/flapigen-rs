///missed stuff in syntex
use syntex_syntax::parse::{token, parser};
use syntex_syntax::codemap;
use syntex_syntax::{parse, ast};
use syntex_syntax::ptr::P;
use syntex_syntax::parse::common::SeqSep;
use syntex_syntax::parse::token::keywords;
use syntex_syntax::ast::{SelfKind, Mutability};
use syntex_pos::mk_sp;


/// Returns the parsed optional self argument and whether a self shortcut was used.
fn parse_self_arg<'a>(parser: &mut parser::Parser<'a>) -> parse::PResult<'a, Option<ast::Arg>> {
    let expect_ident = |this: &mut parser::Parser<'a>| match this.token {
        // Preserve hygienic context.
        token::Ident(ident) => { this.bump(); codemap::respan(this.prev_span, ident) }
        _ => unreachable!()
    };

    // Parse optional self parameter of a method.
    // Only a limited set of initial token sequences is considered self parameters, anything
    // else is parsed as a normal function parameter list, so some lookahead is required.
    let eself_lo = parser.span.lo;
    let (eself, eself_ident) = match parser.token {
        token::BinOp(token::And) => {
            // &self
            // &mut self
            // &'lt self
            // &'lt mut self
            // &not_self
            if parser.look_ahead(1, |t| t.is_keyword(keywords::SelfValue)) {
                parser.bump();
                (SelfKind::Region(None, Mutability::Immutable), expect_ident(parser))
            } else if parser.look_ahead(1, |t| t.is_keyword(keywords::Mut)) &&
                parser.look_ahead(2, |t| t.is_keyword(keywords::SelfValue)) {
                    parser.bump();
                    parser.bump();
                    (SelfKind::Region(None, Mutability::Mutable), expect_ident(parser))
                } else if parser.look_ahead(1, |t| t.is_lifetime()) &&
                parser.look_ahead(2, |t| t.is_keyword(keywords::SelfValue)) {
                    parser.bump();
                    let lt = try!(parser.parse_lifetime());
                    (SelfKind::Region(Some(lt), Mutability::Immutable), expect_ident(parser))
                } else if parser.look_ahead(1, |t| t.is_lifetime()) &&
                parser.look_ahead(2, |t| t.is_keyword(keywords::Mut)) &&
                parser.look_ahead(3, |t| t.is_keyword(keywords::SelfValue)) {
                    parser.bump();
                    let lt = try!(parser.parse_lifetime());
                    parser.bump();
                    (SelfKind::Region(Some(lt), Mutability::Mutable), expect_ident(parser))
                } else {
                    return Ok(None);
                }
        }
        token::BinOp(token::Star) => {
            // *self
            // *const self
            // *mut self
            // *not_self
            // Emit special error for `self` cases.
            if parser.look_ahead(1, |t| t.is_keyword(keywords::SelfValue)) {
                parser.bump();
                parser.span_err(parser.span, "cannot pass `self` by raw pointer");
                (SelfKind::Value(Mutability::Immutable), expect_ident(parser))
            } else if parser.look_ahead(1, |t| t.is_mutability()) &&
                parser.look_ahead(2, |t| t.is_keyword(keywords::SelfValue)) {
                    parser.bump();
                    parser.bump();
                    parser.span_err(parser.span, "cannot pass `self` by raw pointer");
                    (SelfKind::Value(Mutability::Immutable), expect_ident(parser))
                } else {
                    return Ok(None);
                }
        }
        token::Ident(..) => {
            if parser.token.is_keyword(keywords::SelfValue) {
                // self
                // self: TYPE
                let eself_ident = expect_ident(parser);
                if parser.eat(&token::Colon) {
                    let ty = try!(parser.parse_ty_sum());
                    (SelfKind::Explicit(ty, Mutability::Immutable), eself_ident)
                } else {
                    (SelfKind::Value(Mutability::Immutable), eself_ident)
                }
            } else if parser.token.is_keyword(keywords::Mut) &&
                parser.look_ahead(1, |t| t.is_keyword(keywords::SelfValue)) {
                    // mut self
                    // mut self: TYPE
                    parser.bump();
                    let eself_ident = expect_ident(parser);
                    if parser.eat(&token::Colon) {
                        let ty = try!(parser.parse_ty_sum());
                        (SelfKind::Explicit(ty, Mutability::Mutable), eself_ident)
                    } else {
                        (SelfKind::Value(Mutability::Mutable), eself_ident)
                    }
                } else {
                    return Ok(None);
                }
        }
        _ => return Ok(None),
    };

    let eself = codemap::respan(mk_sp(eself_lo, parser.prev_span.hi), eself);
    Ok(Some(ast::Arg::from_self(eself, eself_ident)))
}

pub fn parse_fn_decl_with_self<'a, F>(parser: &mut parser::Parser<'a>, parse_arg_fn: F) -> parse::PResult<'a, P<ast::FnDecl>>
    where F: FnMut(&mut parser::Parser<'a>) -> parse::PResult<'a,  ast::Arg>,
{
    try!(parser.expect(&token::OpenDelim(token::Paren)));

    // Parse optional self argument
    let self_arg = try!(parse_self_arg(parser));

    // Parse the rest of the function parameter list.
    let sep = SeqSep::trailing_allowed(token::Comma);
    let fn_inputs = if let Some(self_arg) = self_arg {
        if parser.check(&token::CloseDelim(token::Paren)) {
            vec![self_arg]
        } else if parser.eat(&token::Comma) {
            let mut fn_inputs = vec![self_arg];
            fn_inputs.append(&mut parser.parse_seq_to_before_end(
                &token::CloseDelim(token::Paren), sep, parse_arg_fn)
            );
            fn_inputs
        } else {
            return parser.unexpected();
        }
    } else {
        parser.parse_seq_to_before_end(&token::CloseDelim(token::Paren), sep, parse_arg_fn)
    };

    // Parse closing paren and return type.
    try!(parser.expect(&token::CloseDelim(token::Paren)));
    Ok(P(ast::FnDecl {
        inputs: fn_inputs,
        output: try!(parser.parse_ret_ty()),
        variadic: false
    }))
}
