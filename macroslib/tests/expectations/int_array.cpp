"struct CRustSlicei32 Utils_f(struct CRustSlicei32 a0);";
"static RustSlice<const int32_t> f(RustSlice<const int32_t> a0) noexcept;";
r#"template<bool OWN_DATA>
    inline RustSlice<const int32_t> UtilsWrapper<OWN_DATA>::f(RustSlice<const int32_t> a0) noexcept
    {

        struct CRustSlicei32 ret = Utils_f(a0.as_c<CRustSlicei32>());
        return RustSlice<const int32_t>{ret.data, ret.len};
    }"#;
