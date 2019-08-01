"std::variant<Moo, Foo> f() const";
"Foo f2(Foo a0) const noexcept;";

r#"template<bool OWN_DATA>
    inline std::variant<Moo, Foo> BooWrapper<OWN_DATA>::f() const noexcept
    {

        struct CRustResult4232mut3232c_voidu32 ret = Boo_f(this->self_);
        return ret.is_ok != 0 ?
              std::variant<Moo, Foo> { Moo(static_cast<MooOpaque *>(ret.data.ok)) } :
              std::variant<Moo, Foo> { static_cast<Foo>(ret.data.err) };
    }"#;

r#"template<bool OWN_DATA>
    inline Foo BooWrapper<OWN_DATA>::f2(Foo a0) const noexcept
    {

        uint32_t ret = Boo_f2(this->self_, static_cast<uint32_t>(a0));
        return static_cast<Foo>(ret);
    }"#;
