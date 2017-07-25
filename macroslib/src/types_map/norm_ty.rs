use syntex_syntax::ast::{Ty, TyKind};
use syntex_syntax::print::pprust;


pub(crate) fn normalized_ty_string(ty: &Ty) -> String {
    match &ty.node {
        &TyKind::Rptr(_, ref ity) => {
            pprust::ty_to_string(&Ty {
                                     node: TyKind::Rptr(None, ity.clone()),
                                     ..ty.clone()
                                 })
        }
        _ => pprust::ty_to_string(ty),
    }
}

#[cfg(test)]
mod tests {
    use syntex_syntax::parse;
    use super::*;

    #[test]
    fn test_normalize_ty() {
        let session = parse::ParseSess::new();

        let str_to_ty = |code: &str| {
            let mut parser =
                parse::new_parser_from_source_str(&session, "ty test".into(), code.to_string());
            parser.parse_ty().unwrap().unwrap()
        };

        assert_eq!(&normalized_ty_string(&str_to_ty("&str")), "&str");
        assert_eq!(&normalized_ty_string(&str_to_ty("&'a str")), "&str");
        assert_eq!(&normalized_ty_string(&str_to_ty("string")), "string");
        assert_eq!(&normalized_ty_string(&str_to_ty("()")), "()");
    }
}
