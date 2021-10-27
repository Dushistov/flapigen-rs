"struct CRustResultCRustVecu84232mut3232c_void LocationService_f1(const LocationServiceOpaque * const self);";
"std::variant<RustVecu8, PosErr> f1() const noexcept;";
r#"template<bool OWN_DATA>
    inline std::variant<RustVecu8, PosErr> LocationServiceWrapper<OWN_DATA>::f1() const noexcept
    {

        struct CRustResultCRustVecu84232mut3232c_void ret = LocationService_f1(this->self_);
        return ret.is_ok != 0 ?
              std::variant<RustVecu8, PosErr> { RustVecu8{ret.data.ok} } :
              std::variant<RustVecu8, PosErr> { PosErr(static_cast<PosErrOpaque *>(ret.data.err)) };
    }"#;

"void f2(RustVecu8 p) const noexcept;";

r#"template<bool OWN_DATA>
    inline void LocationServiceWrapper<OWN_DATA>::f2(RustVecu8 p) const noexcept
    {

        LocationService_f2(this->self_, p.release());
    }"#;

"void LocationService_f2(const LocationServiceOpaque * const self, struct CRustVecu8 p);";

"virtual void on_have_data(RustVecu8 data) noexcept = 0;";
"static void c_on_have_data(struct CRustVecu8 data, void *opaque)";
