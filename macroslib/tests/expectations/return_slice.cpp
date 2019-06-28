"CRustSliceU32 f1() const noexcept;";
"struct CRustSliceU32 Boo_f1(const BooOpaque * const self);";
r#"template<bool OWN_DATA>
    inline  CRustSliceU32 BooWrapper<OWN_DATA>::f1() const noexcept
    {

        struct CRustSliceU32 ret = Boo_f1(this->self_);
        return ret;
    }"#;

"struct CRustObjectSlice Boo_f2(const BooOpaque * const self);";
"RustForeignSlice<FooRef> f2() const noexcept;";
r#"template<bool OWN_DATA>
    inline RustForeignSlice<FooRef> BooWrapper<OWN_DATA>::f2() const noexcept
    {

        struct CRustObjectSlice ret = Boo_f2(this->self_);
        return RustForeignSlice<FooRef>{ret};
    }"#;

"CRustSliceUsize f3() const noexcept;";
"struct CRustSliceUsize Boo_f3(const BooOpaque * const self);";
r#"template<bool OWN_DATA>
    inline  CRustSliceUsize BooWrapper<OWN_DATA>::f3() const noexcept
    {

        struct CRustSliceUsize ret = Boo_f3(this->self_);
        return ret;
    }"#;

