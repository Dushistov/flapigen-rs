@@expect {"after":"\n\n#ifdef __c","before":"extern \"C\" {\n#endif\n\n    ","file":"c_Utils.h","kind":"between"}
struct CRustSlicei32 Utils_f(struct CRustSlicei32 a0);
@@end

@@expect {"after":"\n\n};\n\n\n    t","before":"friend class UtilsWrapper<false>;\n\n    ","file":"Utils.hpp","kind":"between"}
static RustSlice<const int32_t> f(RustSlice<const int32_t> a0) noexcept;
@@end

@@expect {"after":"\n\n} // names","before":"static RustSlice<const int32_t> f(RustSlice<const int32_t> a0) noexcept;\n\n};\n\n\n    ","file":"Utils.hpp","kind":"between"}
template<bool OWN_DATA>
    inline RustSlice<const int32_t> UtilsWrapper<OWN_DATA>::f(RustSlice<const int32_t> a0) noexcept
    {

        struct CRustSlicei32 ret = Utils_f(a0.as_c<CRustSlicei32>());
        return RustSlice<const int32_t>{ret};
    }
@@end
