"struct CRustSlicei32 Utils_f(struct CRustSlicei32 a0);";
"static RustSlice<int32_t> f(RustSlice<int32_t> a0) noexcept;";
r#"template<bool OWN_DATA>
    inline RustSlice<int32_t> UtilsWrapper<OWN_DATA>::f(RustSlice<int32_t> a0) noexcept
    {

        struct CRustSlicei32 ret = Utils_f(a0.as_c<CRustSlicei32>());
        return RustSlice<int32_t>{ret.data, ret.len};
    }"#;
