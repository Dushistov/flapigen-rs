use syntex_syntax::ast;
use syntex_syntax::ptr::P;

pub(crate) fn path_match(path: &ast::Path, type_name: &str) -> bool {
    path.segments
        .last()
        .map(|v| &*v.identifier.name.as_str() == type_name)
        .unwrap_or(false)
}

pub(crate) fn is_type_name(ty: &ast::Ty, type_name: &str) -> bool {
    match ty.node {
        ast::TyKind::Path(_ /*self info*/, ref path) => {
            debug!(
                "is_type_name_result: path: {:?}, ident {:?}",
                path.segments,
                path.segments[0].identifier.name.as_str()
            );
            path_match(path, type_name)
        }
        _ => false,
    }
}

pub(crate) fn path_unpack_generic_first_parameter(
    path: &ast::Path,
    generic_name: &str,
) -> Option<ast::Ty> {
    path.segments
        .first()
        .map(|ps: &ast::PathSegment| {
            if &*ps.identifier.name.as_str() == generic_name {
                ps.parameters
                    .as_ref()
                    .map(|p: &P<ast::PathParameters>| {
                        if let ast::PathParameters::AngleBracketed(ref params) = **p {
                            params.types.first().map(|v: &P<ast::Ty>| {
                                debug!("unpack_generic_first_paramter: result param {:?}", *v);
                                (**v).clone()
                            })
                        } else {
                            None
                        }
                    })
                    .unwrap_or(None)
            } else {
                None
            }
        })
        .unwrap_or(None)
}

pub(crate) fn unpack_generic_first_parameter(ty: &ast::Ty, generic_name: &str) -> ast::Ty {
    match ty.node {
        ast::TyKind::Path(_ /*self info*/, ref path) => {
            debug!(
                "unpack_generic_first_paramter: path: {:?}, ident {:?}",
                path.segments,
                path.segments[0].identifier.name.as_str()
            );
            path_unpack_generic_first_parameter(path, generic_name).unwrap_or_else(|| ty.clone())
        }
        _ => ty.clone(),
    }
}

pub(crate) fn unpack_first_associated_type(
    items: &[ast::ImplItem],
    type_name: &str,
) -> Option<ast::Ty> {
    for item in items {
        let name: &str = &(item.ident.name.as_str());
        if name == type_name {
            if let ast::ImplItemKind::Type(ref ty) = item.node {
                return Some((**ty).clone());
            }
        }
    }
    None
}


#[cfg(test)]
mod tests {
    use super::*;
    use syntex_syntax::parse;
    use syntex_syntax::print::pprust;

    #[test]
    fn test_is_type_name() {
        let session = parse::ParseSess::new();
        let mut parser = parse::new_parser_from_source_str(
            &session,
            "test".into(),
            "Result<Foo, String>".into(),
        );
        let ty = parser.parse_ty().unwrap();
        assert!(is_type_name(&*ty, "Result"));
    }

    #[test]
    fn test_unpack_generic_first_paramter() {
        let session = parse::ParseSess::new();
        let mut parser = parse::new_parser_from_source_str(
            &session,
            "test".into(),
            "Result<Foo, String>".into(),
        );
        let ty = parser.parse_ty().unwrap();
        assert!(is_type_name(&*ty, "Result"));

        let ok_ty = unpack_generic_first_parameter(&*ty, "Result");
        assert!(!is_type_name(&ok_ty, "Result"));
        assert_eq!(pprust::ty_to_string(&ok_ty), "Foo");

        let ty = parse::new_parser_from_source_str(&session, "test".into(), "Vec<Foo>".into())
            .parse_ty()
            .unwrap();
        assert!(is_type_name(&ty, "Vec"));
        let in_ty = unpack_generic_first_parameter(&*ty, "Vec");
        assert_eq!(pprust::ty_to_string(&in_ty), "Foo");
    }
}
