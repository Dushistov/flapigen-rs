r##"struct CRustSliceMutForeignFoo {
    struct FooOpaque * data;
    uintptr_t len;
};"##;
r##"struct CRustSliceForeignFoo {
    const struct FooOpaque * data;
    uintptr_t len;
};"##;

"struct CRustSliceu32 Boo_f1(const BooOpaque * const self, struct CRustSliceMutForeignFoo a0);";
"RustSlice<const uint32_t> f1(RustSlice<Foo> a0) const noexcept;";
r#"template<bool OWN_DATA>
    inline RustSlice<const uint32_t> BooWrapper<OWN_DATA>::f1(RustSlice<Foo> a0) const noexcept
    {

        struct CRustSliceu32 ret = Boo_f1(this->self_, a0.as_c<CRustSliceMutForeignFoo>());
        return RustSlice<const uint32_t>{ret};
    }"#;

"struct CRustSliceu32 Boo_f2(const BooOpaque * const self, struct CRustSliceForeignFoo a0);";
"RustSlice<const uint32_t> f2(RustSlice<const Foo> a0) const noexcept;";
r#"template<bool OWN_DATA>
    inline RustSlice<const uint32_t> BooWrapper<OWN_DATA>::f2(RustSlice<const Foo> a0) const noexcept
    {

        struct CRustSliceu32 ret = Boo_f2(this->self_, a0.as_c<CRustSliceForeignFoo>());
        return RustSlice<const uint32_t>{ret};
    }"#;
