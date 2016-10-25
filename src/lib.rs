extern crate syntex;
extern crate syntex_syntax;
extern crate syntex_pos;
extern crate aster;

use syntex::Registry;
use syntex_syntax::ext::base::{ExtCtxt, MacResult, DummyResult, MacEager};
use syntex_syntax::parse::{token, parser};
use syntex_syntax::tokenstream::TokenTree;
use syntex_syntax::codemap::Span;
use syntex_syntax::codemap;
use syntex_syntax::{parse, ast};
use syntex_syntax::ptr::P;
use syntex_syntax::parse::common::SeqSep;
use syntex_syntax::parse::token::keywords;
use syntex_syntax::ast::{SelfKind, Mutability};
use syntex_pos::mk_sp;
use syntex_syntax::util::small_vector::SmallVector;

enum FuncVariant {
    Constructor, Method, StaticMethod
}

impl FuncVariant {
    fn from_str(ident: &token::InternedString) -> Option<FuncVariant> {
        if ident == "constructor" {
            Some(FuncVariant::Constructor)
        } else if ident == "method" {
            Some(FuncVariant::Method)
        } else if ident == "static_method" {
            Some(FuncVariant::StaticMethod)
        } else {
            None
        }
    }
}

pub fn register(registry: &mut Registry) {
    registry.add_macro("foreigner_class", expand_foreigner_class);
}

/// Returns the parsed optional self argument and whether a self shortcut was used.
fn parse_self_arg<'a>(parser: &mut parser::Parser<'a>) -> parse::PResult<'a, Option<ast::Arg>> {
    let expect_ident = |this: &mut parser::Parser<'a>| match this.token {
        // Preserve hygienic context.
        token::Ident(ident) => { this.bump(); codemap::respan(this.last_span, ident) }
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

    let eself = codemap::respan(mk_sp(eself_lo, parser.last_span.hi), eself);
    Ok(Some(ast::Arg::from_self(eself, eself_ident)))
}

fn parse_fn_decl_with_self<'a, F>(parser: &mut parser::Parser<'a>, parse_arg_fn: F) -> parse::PResult<'a, P<ast::FnDecl>>
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


fn expand_foreigner_class<'cx>(cx: &'cx mut ExtCtxt,
                               sp: Span,
                               tokens: &[TokenTree])
                               -> Box<MacResult + 'cx> {
    let mut parser = parse::new_parser_from_tts(cx.parse_sess, cx.cfg.clone(), tokens.to_vec());
    let class_ident = parser.parse_ident().unwrap();
    if class_ident.name.as_str() != "class" {
        println!("class_indent {:?}", class_ident);
        cx.span_err(parser.span, "expect class here");
        return DummyResult::any(parser.span);
    }
    let class_name_indent = parser.parse_ident().unwrap();
    println!("CLASS NAME {:?}", class_name_indent);
    parser.expect(&token::Token::OpenDelim(token::DelimToken::Brace)).unwrap();
    let mut methods = Vec::<(FuncVariant, ast::Path, P<ast::FnDecl>)>::new();
    loop {
        if parser.eat(&token::Token::CloseDelim(token::DelimToken::Brace)) {
            break;
        }
        let func_type_name = parser.parse_ident().unwrap();
        println!("func_type {:?}", func_type_name);
        let func_type = FuncVariant::from_str(&func_type_name.name.as_str());
        if func_type.is_none() {
            println!("unknown func type: {:?}", func_type_name);
            cx.span_err(parser.span, "expect 'constructor' or 'method' or 'static_method' here");
            return DummyResult::any(parser.span);
        }
        let func_type = func_type.unwrap();
        let func_name = parser.parse_path(parser::PathStyle::Mod).unwrap();
        println!("func_name {:?}", func_name);

        let func_decl = match func_type {
            FuncVariant::Constructor | FuncVariant::StaticMethod => parser.parse_fn_decl(false).unwrap(),
            FuncVariant::Method => parse_fn_decl_with_self(&mut parser, |p| p.parse_arg()).unwrap(),
        };
        println!("func_decl {:?}", func_decl);
        parser.expect(&token::Token::Semi).unwrap();

        methods.push((func_type, func_name, func_decl));
    }
    let package_name = "example_com";
    let builder = ::aster::AstBuilder::new();
    let mut jni_methods = Vec::new();
    for it in methods.iter() {
        match it.0 {
            FuncVariant::Constructor | FuncVariant::StaticMethod => (),
            FuncVariant::Method => {
                let block = builder.block()
                    .stmt().let_id("x").isize(1)
                    .stmt().let_id("y").isize(2)
                    .expr().add().id("x").id("y");
                let func_name = match it.1.segments.len() {
                    0 => token::InternedString::new(""),
                    n => it.1.segments[n - 1].identifier.name.as_str(),
                };
                let mut fn_ = builder.item().fn_(format!("Java_{}_{}_{}", package_name,
                                                     class_name_indent.name.as_str(),
                                                     func_name
                                                     ))
                    .return_().isize()
                    .build(block.clone());
                let no_mangle_attr = builder.attr().word("no_mangle");
                let fn_ = fn_.map(|mut p| {p.attrs.push(no_mangle_attr); p });
                jni_methods.push(fn_);
            }
        }
    }

    MacEager::items(SmallVector::many(jni_methods))
}
