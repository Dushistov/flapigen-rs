"RustForeignVecFoo get_foo_arr() const noexcept;";
"struct CRustForeignVec Boo_get_foo_arr(const BooOpaque * const self);";
r#"template<bool OWN_DATA>
    inline RustForeignVecFoo BooWrapper<OWN_DATA>::get_foo_arr() const noexcept
    {

        struct CRustForeignVec ret = Boo_get_foo_arr(this->self_);
        return RustForeignVecFoo{ret};
    }"#;


"struct CRustResult4232mut3232c_voidCRustString Boo_get_foo_with_err(const BooOpaque * const self);";
"std::variant<Foo, RustString> get_foo_with_err() const noexcept;";
r#"template<bool OWN_DATA>
    inline std::variant<Foo, RustString> BooWrapper<OWN_DATA>::get_foo_with_err() const noexcept
    {

        struct CRustResult4232mut3232c_voidCRustString ret = Boo_get_foo_with_err(this->self_);
        return ret.is_ok != 0 ?
              std::variant<Foo, RustString> { Foo(static_cast<FooOpaque *>(ret.data.ok)) } :
              std::variant<Foo, RustString> { RustString{ret.data.err} };
    }"#;


"struct CRustResultCRustForeignVecCRustString Boo_get_foo_arr_with_err(const BooOpaque * const self);";
"std::variant<RustForeignVecFoo, RustString> get_foo_arr_with_err() const noexcept;";
r#"template<bool OWN_DATA>
    inline std::variant<RustForeignVecFoo, RustString> BooWrapper<OWN_DATA>::get_foo_arr_with_err() const noexcept
    {

        struct CRustResultCRustForeignVecCRustString ret = Boo_get_foo_arr_with_err(this->self_);
        return ret.is_ok != 0 ?
              std::variant<RustForeignVecFoo, RustString> { RustForeignVecFoo{ret.data.ok} } :
              std::variant<RustForeignVecFoo, RustString> { RustString{ret.data.err} };
    }"#;
