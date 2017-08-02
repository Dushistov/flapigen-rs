///missed stuff in syntex
use syntex_syntax::parse::{token, PResult};
use syntex_syntax::codemap;
use syntex_syntax::parse;
use syntex_syntax::ptr::P;
use syntex_syntax::parse::common::SeqSep;
use syntex_syntax::symbol::keywords;
use syntex_syntax::ast;
use syntex_syntax::ast::{SelfKind, Mutability, Arg, FnDecl};
use syntex_pos::mk_sp;
use syntex_syntax::parse::parser::Parser;
use syntex_syntax::ext::base::ExtCtxt;
use syntex_syntax::tokenstream::TokenTree;
use syntex_pos::Span;
use syntex_syntax::parse::parser;

use ForeignerClassInfo;
use MethodVariant;
use ForeignerMethod;
use my_ast::{is_type_name, unpack_generic_first_parameter};
use types_map::norm_ty::normalized_ty_string;

/// Returns the parsed optional self argument and whether a self shortcut was used.
fn parse_self_arg<'a>(parser: &mut Parser<'a>) -> parse::PResult<'a, Option<Arg>> {
    let expect_ident = |this: &mut Parser<'a>| match this.token {
        // Preserve hygienic context.
        token::Ident(ident) => {
            this.bump();
            codemap::respan(this.prev_span, ident)
        }
        _ => unreachable!(),
    };
    let isolated_self = |this: &mut Parser<'a>, n| {
        this.look_ahead(n, |t| t.is_keyword(keywords::SelfValue)) &&
        this.look_ahead(n + 1, |t| t != &token::ModSep)
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
            if isolated_self(parser, 1) {
                parser.bump();
                (SelfKind::Region(None, Mutability::Immutable), expect_ident(parser))
            } else if parser.look_ahead(1, |t| t.is_keyword(keywords::Mut)) &&
                      isolated_self(parser, 2) {
                parser.bump();
                parser.bump();
                (SelfKind::Region(None, Mutability::Mutable), expect_ident(parser))
            } else if parser.look_ahead(1, |t| t.is_lifetime()) && isolated_self(parser, 2) {
                parser.bump();
                let lt = try!(parser.parse_lifetime());
                (SelfKind::Region(Some(lt), Mutability::Immutable), expect_ident(parser))
            } else if parser.look_ahead(1, |t| t.is_lifetime()) &&
                      parser.look_ahead(2, |t| t.is_keyword(keywords::Mut)) &&
                      isolated_self(parser, 3) {
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
            if isolated_self(parser, 1) {
                parser.bump();
                parser.span_err(parser.span, "cannot pass `self` by raw pointer");
                (SelfKind::Value(Mutability::Immutable), expect_ident(parser))
            } else if parser.look_ahead(1, |t| t.is_mutability()) && isolated_self(parser, 2) {
                parser.bump();
                parser.bump();
                parser.span_err(parser.span, "cannot pass `self` by raw pointer");
                (SelfKind::Value(Mutability::Immutable), expect_ident(parser))
            } else {
                return Ok(None);
            }
        }
        token::Ident(..) => {
            if isolated_self(parser, 0) {
                // self
                // self: TYPE
                let eself_ident = expect_ident(parser);
                if parser.eat(&token::Colon) {
                    let ty = try!(parser.parse_ty());
                    (SelfKind::Explicit(ty, Mutability::Immutable), eself_ident)
                } else {
                    (SelfKind::Value(Mutability::Immutable), eself_ident)
                }
            } else if parser.token.is_keyword(keywords::Mut) && isolated_self(parser, 1) {
                // mut self
                // mut self: TYPE
                parser.bump();
                let eself_ident = expect_ident(parser);
                if parser.eat(&token::Colon) {
                    let ty = try!(parser.parse_ty());
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
    Ok(Some(Arg::from_self(eself, eself_ident)))
}

/// Parse the parameter list and result type of a function that may have a `self` parameter.
fn parse_fn_decl_with_self<'a, F>(parser: &mut Parser<'a>,
                                  parse_arg_fn: F)
                                  -> PResult<'a, P<FnDecl>>
    where F: FnMut(&mut Parser<'a>) -> PResult<'a, Arg>
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
            fn_inputs.append(&mut parser.parse_seq_to_before_end(&token::CloseDelim(token::Paren),
                                                                 sep,
                                                                 parse_arg_fn));
            fn_inputs
        } else {
            return parser.unexpected();
        }
    } else {
        parser.parse_seq_to_before_end(&token::CloseDelim(token::Paren), sep, parse_arg_fn)
    };

    // Parse closing paren and return type.
    try!(parser.expect(&token::CloseDelim(token::Paren)));
    Ok(P(FnDecl {
             inputs: fn_inputs,
             output: try!(parser.parse_ret_ty()),
             variadic: false,
         }))
}

pub(crate) fn parse_foreigner_class(cx: &ExtCtxt,
                                    tokens: &[TokenTree])
                                    -> Result<ForeignerClassInfo, Span> {
    let class_keyword = ast::Ident::from_str("class");
    let alias_keyword = ast::Ident::from_str("alias");
    let private_keyword = ast::Ident::from_str("private");

    let mut parser = parse::new_parser_from_tts(cx.parse_sess, tokens.to_vec());
    if !parser.eat_contextual_keyword(class_keyword) {
        cx.span_err(parser.span, "expect class keyword here");
        return Err(parser.span);
    }

    let class_name_indent = parser.parse_ident().unwrap();
    debug!("CLASS NAME {:?}", class_name_indent);

    parser
        .expect(&token::Token::OpenDelim(token::DelimToken::Brace))
        .unwrap();
    let mut methods = Vec::new();
    let mut rust_self_type = ast::Path {
        span: parser.span,
        segments: Vec::new(),
    };
    let mut constructor_ret_type: Option<ast::Ty> = None;
    let mut this_type_for_method: Option<ast::Ty> = None;
    let mut foreigner_code = String::new();
    loop {
        if parser.eat(&token::Token::CloseDelim(token::DelimToken::Brace)) {
            break;
        }
        let private_func = parser.eat_contextual_keyword(private_keyword);
        let func_type_name = parser.parse_ident().unwrap();
        debug!("func_type {:?}", func_type_name);
        if &*func_type_name.name.as_str() == "self_type" {
            rust_self_type = parser
                .parse_path(parser::PathStyle::Type)
                .expect("Can not parse self_type");
            debug!("self_type: {:?}", rust_self_type);
            parser.expect(&token::Token::Semi).unwrap();
            continue;
        }

        if &*func_type_name.name.as_str() == "foreigner_code" {
            let lit = parser
                .parse_lit()
                .expect("expect literal after foreigner_code");
            match lit.node {
                ast::LitKind::Str(s, _) => {
                    debug!("foreigner_code s: {:?}", s);
                    foreigner_code.push_str(&*s.as_str());
                }
                _ => {
                    cx.span_err(parser.span, "expect string literal after foreigner_code");
                    return Err(parser.span);
                }
            }
            parser.expect(&token::Token::Semi).unwrap();
            continue;
        }

        let func_type = match MethodVariant::from_ident(&func_type_name.name.as_str()) {
            Some(x) => x,
            None => {
                cx.span_err(parser.span,
                            &format!("expect 'constructor' or 'method' or \
                                      'static_method' here, got: {}",
                                     func_type_name));
                return Err(parser.span);
            }
        };
        let func_name = parser.parse_path(parser::PathStyle::Mod).unwrap();
        debug!("func_name {:?}", func_name);

        let func_decl = match func_type {
            MethodVariant::Constructor |
            MethodVariant::StaticMethod => parser.parse_fn_decl(false).unwrap(),
            MethodVariant::Method => {
                parse_fn_decl_with_self(&mut parser, |p| p.parse_arg()).unwrap()
            }
        };
        debug!("func_decl {:?}", func_decl);
        parser.expect(&token::Token::Semi).unwrap();
        let mut func_name_alias = None;
        if parser.eat_contextual_keyword(alias_keyword) {
            if func_type == MethodVariant::Constructor {
                cx.span_err(parser.span, "alias not supported for 'constructor'");
                return Err(parser.span);
            }
            func_name_alias = Some(parser.parse_ident().unwrap());
            debug!("we have ALIAS `{:?}`", func_name_alias.unwrap());
            parser
                .expect(&token::Token::Semi)
                .expect("no ; at the end of alias");
        }
        let (may_return_error, ret_type) = match func_decl.output {
            ast::FunctionRetTy::Default(_) => (false, None),
            ast::FunctionRetTy::Ty(ref ret_type) => {
                (is_type_name(ret_type, "Result"), Some(ret_type.clone()))
            }
        };
        if let MethodVariant::Constructor = func_type {
            let ret_type = match ret_type {
                Some(x) => x,
                None => {
                    cx.span_err(parser.span,
                                &format!("{}: constructor should return value", class_name_indent));
                    return Err(parser.span);
                }
            };
            if let Some(ref constructor_ret_type) = constructor_ret_type {
                if normalized_ty_string(constructor_ret_type) != normalized_ty_string(&*ret_type) {
                    cx.span_err(parser.span,
                                &format!("mismatched types of construtors: {:?} {:?}",
                                         constructor_ret_type,
                                         ret_type));
                    return Err(parser.span);
                }
            } else {
                constructor_ret_type = Some(ret_type.unwrap());
                this_type_for_method =
                    Some(unpack_generic_first_parameter(constructor_ret_type.as_ref().unwrap(),
                                                        "Result"));
            }
        }
        methods.push(ForeignerMethod {
                         variant: func_type,
                         rust_id: func_name,
                         fn_decl: func_decl,
                         name_alias: func_name_alias.map(|v| v.name),
                         may_return_error: may_return_error,
                         foreigner_private: private_func,
                     });
    }


    Ok(ForeignerClassInfo {
           name: class_name_indent.name.as_str(),
           methods: methods,
           self_type: rust_self_type,
           this_type_for_method: this_type_for_method,
           foreigner_code: foreigner_code,
           constructor_ret_type: constructor_ret_type,
       })
}
