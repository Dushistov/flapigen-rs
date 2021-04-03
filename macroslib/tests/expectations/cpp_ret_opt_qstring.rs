type QString = String;
type QRustStrView<'a> = &'a str;

foreign_typemap!(
    ($p:r_type) QRustStrView => CRustStrView {
        $out = CRustStrView::from_str($p);
    };
    ($p:f_type, req_modules = ["<QString>"], unique_prefix="/*&str*/")
        => "/*&str*/QString" r#"
          QString::fromUtf8($p.data, static_cast<int>($p.len))
"#;
);

foreign_typemap!(
    ($p:r_type) QString => CRustString {
        $out = CRustString::from_string($p);
    };
    ($p:f_type, req_modules = ["<QString>"]) => "QString" r#"
          [](auto v) -> QString {
             const auto qs =  QString::fromUtf8(v.data, static_cast<int>(v.len));
             crust_string_free(v);
             return qs;
          }($p)
"#;
);

foreign_class!(
    #[derive(PlainClass, camelCaseAliases)]
    class RelativePath {
        self_type RelativePath;
        private constructor = empty;

        fn IcaoParameters::get_other_info(&self) -> Option<QRustStrView>;
        fn to_string(&self) -> Option<QString> {
            serde_json::to_string(this)
        }
    }
);
