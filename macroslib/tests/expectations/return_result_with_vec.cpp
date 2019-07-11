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
