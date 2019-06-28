"CRustSliceU32 f1(RustForeignSlice<FooRef> a0) const";
"struct CRustSliceU32 Boo_f1(const BooOpaque * const self, struct CRustObjectSlice a0);";
r#"template<bool OWN_DATA>
    inline  CRustSliceU32 BooWrapper<OWN_DATA>::f1(RustForeignSlice<FooRef> a0) const noexcept
    {

        struct CRustSliceU32 ret = Boo_f1(this->self_, a0);
        return ret;
    }"#;

"struct CRustSliceU32 Boo_f2(const BooOpaque * const self, struct CRustObjectSlice a0);";
"CRustSliceU32 f2(RustForeignSlice<FooRef> a0) const";
r#"template<bool OWN_DATA>
    inline  CRustSliceU32 BooWrapper<OWN_DATA>::f2(RustForeignSlice<FooRef> a0) const noexcept
    {

        struct CRustSliceU32 ret = Boo_f2(this->self_, a0);
        return ret;
    }"#;
