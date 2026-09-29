@@expect {"after":"\n    uintptr","before":"extern \"C\" {\n#endif\n","file":"rust_vec_u8.h","kind":"between"}
struct CRustVecu8 {
    uint8_t * data;
@@end

@@expect {"after":"\n\n    void L","before":"LocationServiceOpaque *LocationService_new();\n\n    ","file":"c_LocationService.h","kind":"between"}
struct CRustResultCRustVecu84232mut3232c_void LocationService_f1(const LocationServiceOpaque * const self);
@@end

@@expect {"after":"\n\n    void f","before":"            std::abort();\n        }\n    }\n\n    ","file":"LocationService.hpp","kind":"between"}
std::variant<RustVecu8, PosErr> f1() const noexcept;
@@end

@@expect {"after":"\n\n    templa","before":"constexpr const uintptr_t &LocationServiceWrapper<OWN_DATA>::rust_elem_size;\n\n\n    ","file":"LocationService.hpp","kind":"between"}
template<bool OWN_DATA>
    inline std::variant<RustVecu8, PosErr> LocationServiceWrapper<OWN_DATA>::f1() const noexcept
    {

        struct CRustResultCRustVecu84232mut3232c_void ret = LocationService_f1(this->self_);
        return ret.is_ok != 0 ?
              std::variant<RustVecu8, PosErr> { RustVecu8{ret.data.ok} } :
              std::variant<RustVecu8, PosErr> { PosErr(static_cast<PosErrOpaque *>(ret.data.err)) };
    }
@@end

@@expect {"after":"\n\nprivate:\n ","before":"std::variant<RustVecu8, PosErr> f1() const noexcept;\n\n    ","file":"LocationService.hpp","kind":"between"}
void f2(RustVecu8 p) const noexcept;
@@end

@@expect {"after":"\n\n} // namespace org_exa","before":"std::variant<RustVecu8, PosErr> { PosErr(static_cast<PosErrOpaque *>(ret.data.err)) };\n    }\n\n    ","file":"LocationService.hpp","kind":"between"}
template<bool OWN_DATA>
    inline void LocationServiceWrapper<OWN_DATA>::f2(RustVecu8 p) const noexcept
    {

        LocationService_f2(this->self_, p.release());
    }
@@end

@@expect {"after":"\n\n    void L","before":"struct CRustResultCRustVecu84232mut3232c_void LocationService_f1(const LocationServiceOpaque * const self);\n\n    ","file":"c_LocationService.h","kind":"between"}
void LocationService_f2(const LocationServiceOpaque * const self, struct CRustVecu8 p);
@@end

@@expect {"after":"\n\n\n    stati","before":"virtual ~IXyz() noexcept {}\n\n    ","file":"IXyz.hpp","kind":"between"}
virtual void on_have_data(RustVecu8 data) noexcept = 0;
@@end

@@expect {"after":"\n    {\n     ","before":"delete p;\n    }\n\n    ","file":"IXyz.hpp","kind":"between"}
static void c_on_have_data(struct CRustVecu8 data, void *opaque)
@@end
