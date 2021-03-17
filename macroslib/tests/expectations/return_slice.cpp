"RustSlice<const uint32_t> f1() const noexcept;";
"struct CRustSliceu32 Boo_f1(const BooOpaque * const self);";
r#"template<bool OWN_DATA>
    inline RustSlice<const uint32_t> BooWrapper<OWN_DATA>::f1() const noexcept
    {

        struct CRustSliceu32 ret = Boo_f1(this->self_);
        return RustSlice<const uint32_t>{ret.data, ret.len};
    }"#;

"struct CRustObjectSlice Boo_f2(const BooOpaque * const self);";
"RustForeignSliceConst<FooRef> f2() const noexcept;";
r#"template<bool OWN_DATA>
    inline RustForeignSliceConst<FooRef> BooWrapper<OWN_DATA>::f2() const noexcept
    {

        struct CRustObjectSlice ret = Boo_f2(this->self_);
        return RustForeignSliceConst<FooRef>{ret};
    }"#;

"RustSlice<const uintptr_t> f3() const noexcept;";
"struct CRustSliceusize Boo_f3(const BooOpaque * const self);";
r#"template<bool OWN_DATA>
    inline RustSlice<const uintptr_t> BooWrapper<OWN_DATA>::f3() const noexcept
    {

        struct CRustSliceusize ret = Boo_f3(this->self_);
        return RustSlice<const uintptr_t>{ret.data, ret.len};
    }"#;

"RustSlice<uintptr_t> f4() const noexcept;";
"struct CRustSliceMutusize Boo_f4(const BooOpaque * const self);";
r#"template<bool OWN_DATA>
    inline RustSlice<uintptr_t> BooWrapper<OWN_DATA>::f4() const noexcept
    {

        struct CRustSliceMutusize ret = Boo_f4(this->self_);
        return RustSlice<uintptr_t>{ret.data, ret.len};
    }"#;
