foreign_typemap!(
    ($p:r_type) QString => CRustString {
        $out = CRustString::from_string($p);
    };
    ($p:f_type, req_modules = ["<QString>"]) => "QString" r#"
          $out = QString::fromUtf8($p.data, $p.len);
          crust_string_free($p);
"#;
);

foreigner_class!(
    #[derive(Copy)]
    class BtAddr {
    self_type BtAddr;
    private constructor = empty;
    fn BtAddr::to_string(&self) -> QString;
    fn BtAddr::clone(&self) -> BtAddr;
});
