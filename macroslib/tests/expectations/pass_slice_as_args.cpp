"RustSlice<uint32_t> f1(RustForeignSlice<FooRef, CRustObjectMutSlice> a0) const noexcept;";
"struct CRustSliceu32 Boo_f1(const BooOpaque * const self, struct CRustObjectMutSlice a0);";
r#"template<bool OWN_DATA>
    inline RustSlice<uint32_t> BooWrapper<OWN_DATA>::f1(RustForeignSlice<FooRef, CRustObjectMutSlice> a0) const noexcept
    {

        struct CRustSliceu32 ret = Boo_f1(this->self_, a0);
        return RustSlice<uint32_t>{ret.data, ret.len};
    }"#;

"struct CRustSliceu32 Boo_f2(const BooOpaque * const self, struct CRustObjectSlice a0);";
"RustSlice<uint32_t> f2(RustForeignSlice<FooRef, CRustObjectSlice> a0) const noexcept;";
r#"template<bool OWN_DATA>
    inline RustSlice<uint32_t> BooWrapper<OWN_DATA>::f2(RustForeignSlice<FooRef, CRustObjectSlice> a0) const noexcept
    {

        struct CRustSliceu32 ret = Boo_f2(this->self_, a0);
        return RustSlice<uint32_t>{ret.data, ret.len};
    }"#;
