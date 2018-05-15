use syntex_errors::DiagnosticBuilder;
use syntex_pos::{mk_sp, Span, DUMMY_SP};
use syntex_syntax::ast::{Arg, FnDecl, Mutability, SelfKind};
use syntex_syntax::ext::base::ExtCtxt;
use syntex_syntax::parse::common::SeqSep;
use syntex_syntax::parse::parser::Parser;
use syntex_syntax::parse::{parser, token, PResult};
use syntex_syntax::ptr::P;
use syntex_syntax::symbol::{keywords, Symbol};
use syntex_syntax::tokenstream::TokenTree;
use syntex_syntax::{ast, codemap, parse};

use my_ast::{if_result_return_ok_err_types, normalized_ty_string, self_variant};
use {ForeignEnumInfo, ForeignEnumItem, ForeignInterface, ForeignInterfaceMethod,
     ForeignerClassInfo, ForeignerMethod, MethodVariant, SelfTypeVariant};

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
        this.look_ahead(n, |t| t.is_keyword(keywords::SelfValue)) && this.look_ahead(n + 1, |t| {
            t != &token::ModSep
        })
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
                (
                    SelfKind::Region(None, Mutability::Immutable),
                    expect_ident(parser),
                )
            } else if parser.look_ahead(1, |t| t.is_keyword(keywords::Mut))
                && isolated_self(parser, 2)
            {
                parser.bump();
                parser.bump();
                (
                    SelfKind::Region(None, Mutability::Mutable),
                    expect_ident(parser),
                )
            } else if parser.look_ahead(1, |t| t.is_lifetime()) && isolated_self(parser, 2) {
                parser.bump();
                let lt = try!(parser.parse_lifetime());
                (
                    SelfKind::Region(Some(lt), Mutability::Immutable),
                    expect_ident(parser),
                )
            } else if parser.look_ahead(1, |t| t.is_lifetime())
                && parser.look_ahead(2, |t| t.is_keyword(keywords::Mut))
                && isolated_self(parser, 3)
            {
                parser.bump();
                let lt = try!(parser.parse_lifetime());
                parser.bump();
                (
                    SelfKind::Region(Some(lt), Mutability::Mutable),
                    expect_ident(parser),
                )
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
fn parse_fn_decl_with_self<'a, F>(
    parser: &mut Parser<'a>,
    parse_arg_fn: F,
) -> PResult<'a, P<FnDecl>>
where
    F: FnMut(&mut Parser<'a>) -> PResult<'a, Arg>,
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
                &token::CloseDelim(token::Paren),
                sep,
                parse_arg_fn,
            ));
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

pub(crate) fn parse_foreigner_class(
    cx: &ExtCtxt,
    tokens: &[TokenTree],
) -> Result<ForeignerClassInfo, Span> {
    let class_keyword = ast::Ident::from_str("class");
    let alias_keyword = ast::Ident::from_str("alias");
    let private_keyword = ast::Ident::from_str("private");

    let constructor_keyword = Symbol::intern("constructor");
    let method_keyword = Symbol::intern("method");
    let static_method_keyword = Symbol::intern("static_method");

    let mut parser = parse::new_parser_from_tts(cx.parse_sess, tokens.to_vec());

    let mut class_doc_comments = vec![];
    while let token::Token::DocComment(comment) = parser.token {
        trace!("parse_foreigner_class: comment {:?}", comment);
        class_doc_comments.push(comment);
        parser.bump();
    }

    if !parser.eat_contextual_keyword(class_keyword) {
        cx.span_err(parser.span, "expect `class` keyword here");
        return Err(parser.span);
    }

    let map_perror = |err: DiagnosticBuilder| -> Span {
        let diag = err.into_diagnostic();
        let primary_span = diag.span.primary_span().unwrap_or(DUMMY_SP);
        cx.parse_sess
            .span_diagnostic
            .span_err(diag.span.clone(), &diag.message());
        primary_span
    };

    let class_name_indent = parser.parse_ident().map_err(&map_perror)?;
    debug!("CLASS NAME {:?}", class_name_indent);
    let class_span = parser.span;

    parser
        .expect(&token::Token::OpenDelim(token::DelimToken::Brace))
        .map_err(&map_perror)?;
    let mut methods = Vec::new();
    let mut rust_self_type = ast::Path {
        span: parser.span,
        segments: Vec::new(),
    };
    let mut constructor_ret_type: Option<ast::Ty> = None;
    let mut this_type_for_method: Option<ast::Ty> = None;
    let mut foreigner_code = String::new();
    while !parser.eat(&token::Token::CloseDelim(token::DelimToken::Brace)) {
        let mut doc_comments = vec![];
        while let token::Token::DocComment(comment) = parser.token {
            trace!("parse_foreigner_class: comment {:?}", comment);
            doc_comments.push(comment);
            parser.bump();
        }

        let private_func = parser.eat_contextual_keyword(private_keyword);
        let func_type_name = parser.parse_ident().map_err(&map_perror)?;
        debug!("func_type {:?}", func_type_name);
        if &*func_type_name.name.as_str() == "self_type" {
            rust_self_type = parser
                .parse_path(parser::PathStyle::Type)
                .map_err(&map_perror)?;
            debug!("self_type: {:?}", rust_self_type);
            parser.expect(&token::Token::Semi).map_err(&map_perror)?;
            continue;
        }

        if &*func_type_name.name.as_str() == "foreigner_code" {
            let lit = parser.parse_lit().map_err(&map_perror)?;
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
            parser.expect(&token::Token::Semi).map_err(&map_perror)?;
            continue;
        }

        let mut func_type = match func_type_name.name {
            _ if func_type_name.name == constructor_keyword => MethodVariant::Constructor,
            _ if func_type_name.name == static_method_keyword => MethodVariant::StaticMethod,
            _ if func_type_name.name == method_keyword => {
                MethodVariant::Method(SelfTypeVariant::Default)
            }
            _ => {
                cx.span_err(
                    parser.span,
                    &format!(
                        "expect 'constructor' or 'method' or \
                         'static_method' here, got: {}",
                        func_type_name
                    ),
                );
                return Err(parser.span);
            }
        };
        let func_name = parser
            .parse_path(parser::PathStyle::Mod)
            .map_err(&map_perror)?;
        debug!("func_name {:?}", func_name);

        parser.parse_generics().map_err(&map_perror)?; //just skip <'a,...> section

        let func_decl = match func_type {
            MethodVariant::Constructor | MethodVariant::StaticMethod => {
                parser.parse_fn_decl(false).map_err(&map_perror)?
            }
            MethodVariant::Method(ref mut self_type) => {
                let fn_decl =
                    parse_fn_decl_with_self(&mut parser, |p| p.parse_arg()).map_err(&map_perror)?;
                if fn_decl.inputs.is_empty() {
                    cx.span_err(
                        parser.span,
                        "No first argument in method (should be self/&self/&mut self/mut self)",
                    );
                    return Err(parser.span);
                }
                *self_type = self_variant(&fn_decl.inputs[0].ty).ok_or_else(|| {
                    cx.span_err(
                        parser.span,
                        &format!("Can not parse type {:?} as self type", fn_decl.inputs[0].ty),
                    );
                    parser.span
                })?;

                fn_decl
            }
        };
        debug!("func_decl {:?}", func_decl);
        parser.expect(&token::Token::Semi).map_err(&map_perror)?;
        let mut func_name_alias = None;
        if parser.eat_contextual_keyword(alias_keyword) {
            if func_type == MethodVariant::Constructor {
                cx.span_err(parser.span, "alias not supported for 'constructor'");
                return Err(parser.span);
            }
            func_name_alias = Some(parser.parse_ident().map_err(&map_perror)?);
            debug!("we have ALIAS `{:?}`", func_name_alias.unwrap());
            parser.expect(&token::Token::Semi).map_err(&map_perror)?;
        }
        let (may_return_error, ret_type) = match func_decl.output {
            ast::FunctionRetTy::Default(_) => (false, None),
            ast::FunctionRetTy::Ty(ref ret_type) => (
                if_result_return_ok_err_types(ret_type).is_some(),
                Some(ret_type.clone()),
            ),
        };
        if let MethodVariant::Constructor = func_type {
            let ret_type = match ret_type {
                Some(x) => x,
                None => {
                    cx.span_err(
                        parser.span,
                        &format!("{}: constructor should return value", class_name_indent),
                    );
                    return Err(parser.span);
                }
            };
            if let Some(ref constructor_ret_type) = constructor_ret_type {
                debug!("second constructor, ret type: {:?}", constructor_ret_type);
                if normalized_ty_string(constructor_ret_type) != normalized_ty_string(&*ret_type) {
                    cx.span_err(
                        parser.span,
                        &format!(
                            "mismatched types of construtors: {:?} {:?}",
                            constructor_ret_type, ret_type
                        ),
                    );
                    return Err(parser.span);
                }
            } else {
                debug!(
                    "first constructor for {}, ret type {:?}",
                    class_name_indent, ret_type
                );
                let ret_type = ret_type.unwrap();
                constructor_ret_type = Some(ret_type.clone());
                this_type_for_method = Some(
                    if_result_return_ok_err_types(constructor_ret_type.as_ref().unwrap())
                        .unwrap_or_else(|| (ret_type.clone(), ret_type))
                        .0,
                );
            }
        }
        methods.push(ForeignerMethod {
            variant: func_type,
            rust_id: func_name,
            fn_decl: func_decl,
            name_alias: func_name_alias.map(|v| v.name),
            may_return_error: may_return_error,
            foreigner_private: private_func,
            doc_comments,
        });
    }

    if cx.parse_sess.span_diagnostic.err_count() > 0 {
        return Err(DUMMY_SP);
    }
    Ok(ForeignerClassInfo {
        name: class_name_indent.name,
        methods,
        self_type: rust_self_type,
        this_type_for_method,
        foreigner_code,
        constructor_ret_type,
        span: class_span,
        doc_comments: class_doc_comments,
    })
}

pub(crate) fn parse_foreign_enum(
    cx: &ExtCtxt,
    tokens: &[TokenTree],
) -> Result<ForeignEnumInfo, Span> {
    let enum_keyword = ast::Ident::from_str("enum");
    let mut parser = parse::new_parser_from_tts(cx.parse_sess, tokens.to_vec());
    let mut enum_doc_comments = vec![];
    while let token::Token::DocComment(comment) = parser.token {
        trace!("parse_foreign_enum: comment {:?}", comment);
        enum_doc_comments.push(comment);
        parser.bump();
    }

    if !parser.eat_contextual_keyword(enum_keyword) {
        cx.span_err(parser.span, "expect `enum` keyword here");
        return Err(parser.span);
    }

    let map_perror = |err: DiagnosticBuilder| -> Span {
        let diag = err.into_diagnostic();
        let primary_span = diag.span.primary_span().unwrap_or(DUMMY_SP);
        cx.parse_sess
            .span_diagnostic
            .span_err(diag.span.clone(), &diag.message());
        primary_span
    };
    let enum_name = parser.parse_ident().map_err(&map_perror)?.name;
    debug!("ENUM NAME {:?}", enum_name);
    let enum_span = parser.span;

    parser
        .expect(&token::Token::OpenDelim(token::DelimToken::Brace))
        .map_err(&map_perror)?;
    let mut items = vec![];
    while !parser.eat(&token::Token::CloseDelim(token::DelimToken::Brace)) {
        let mut doc_comments = vec![];
        while let token::Token::DocComment(comment) = parser.token {
            trace!("parse_foreig_enum: comment {:?}", comment);
            doc_comments.push(comment);
            parser.bump();
        }
        let f_item_name = parser.parse_ident().map_err(&map_perror)?.name;
        let span = parser.span;
        parser.expect(&token::Token::Eq).map_err(&map_perror)?;
        let item_name = parser
            .parse_path(parser::PathStyle::Mod)
            .map_err(&map_perror)?;
        parser.expect(&token::Token::Comma).map_err(&map_perror)?;
        items.push(ForeignEnumItem {
            name: f_item_name,
            span: span,
            rust_name: item_name,
            doc_comments,
        });
    }
    Ok(ForeignEnumInfo {
        span: enum_span,
        name: enum_name,
        items,
        doc_comments: enum_doc_comments,
    })
}

pub(crate) fn parse_foreign_interface(
    cx: &ExtCtxt,
    tokens: &[TokenTree],
) -> Result<ForeignInterface, Span> {
    let interface_keyword = ast::Ident::from_str("interface");
    let mut parser = parse::new_parser_from_tts(cx.parse_sess, tokens.to_vec());
    let mut interface_doc_comments = vec![];
    while let token::Token::DocComment(comment) = parser.token {
        trace!("parse_foreign_interface: comment {:?}", comment);
        interface_doc_comments.push(comment);
        parser.bump();
    }

    if !parser.eat_contextual_keyword(interface_keyword) {
        cx.span_err(parser.span, "expect `interface` keyword here");
        return Err(parser.span);
    }

    let map_perror = |err: DiagnosticBuilder| -> Span {
        let diag = err.into_diagnostic();
        let primary_span = diag.span.primary_span().unwrap_or(DUMMY_SP);
        cx.parse_sess
            .span_diagnostic
            .span_err(diag.span.clone(), &diag.message());
        primary_span
    };
    let interface_name = parser.parse_ident().map_err(&map_perror)?.name;
    debug!("INTERFACE NAME {:?}", interface_name);
    let interface_span = parser.span;

    parser
        .expect(&token::Token::OpenDelim(token::DelimToken::Brace))
        .map_err(&map_perror)?;
    let mut self_type = None;
    let mut items = vec![];
    while !parser.eat(&token::Token::CloseDelim(token::DelimToken::Brace)) {
        let mut doc_comments = vec![];
        while let token::Token::DocComment(comment) = parser.token {
            trace!("parse_foreign_interface: comment {:?}", comment);
            doc_comments.push(comment);
            parser.bump();
        }
        let func_name = parser.parse_ident().map_err(&map_perror)?.name;
        if &*func_name.as_str() == "self_type" {
            self_type = Some(parser
                .parse_path(parser::PathStyle::Type)
                .map_err(&map_perror)?);
            debug!("self_type: {:?} for {}", self_type, interface_name);
            parser.expect(&token::Token::Semi).map_err(&map_perror)?;
            continue;
        }
        parser.expect(&token::Token::Eq).map_err(&map_perror)?;
        let rust_func_name = parser
            .parse_path(parser::PathStyle::Mod)
            .map_err(&map_perror)?;
        let fn_decl = parse_fn_decl_with_self(&mut parser, |p| p.parse_arg()).map_err(&map_perror)?;
        parser.expect(&token::Token::Semi).map_err(&map_perror)?;
        items.push(ForeignInterfaceMethod {
            name: func_name,
            rust_name: rust_func_name,
            fn_decl,
            doc_comments,
        });
    }
    let self_type: ast::Path = self_type.ok_or_else(|| {
        cx.span_err(interface_span, "No `self_type` in foreign_interface");
        interface_span
    })?;
    Ok(ForeignInterface {
        name: interface_name,
        self_type,
        doc_comments: interface_doc_comments,
        items,
        span: interface_span,
    })
}
