r#"public:

    std::pair<One, Two> f() const noexcept"#;
r#""#;

r#"template<bool OWN_DATA>
    inline std::pair<One, Two> FooWrapper<OWN_DATA>::f() const noexcept
    {

        struct CRustPair4232mut3232c_void4232mut3232c_void ret = Foo_f(this->self_);
        return std::make_pair(One(static_cast<OneOpaque *>(ret.first)), Two(static_cast<TwoOpaque *>(ret.second)));
    }"#;

"struct CRustPair4232mut3232c_void4232mut3232c_void Foo_f(const FooOpaque * const self);";

"struct CRustPairi32i32 Foo_g(const FooOpaque * const self);";

r#"template<bool OWN_DATA>
    inline std::pair<int32_t, int32_t> FooWrapper<OWN_DATA>::g() const noexcept
    {

        struct CRustPairi32i32 ret = Foo_g(this->self_);
        return std::make_pair(ret.first, ret.second);
    }"#;

"struct CRustPairCRustStrViewCRustStrView Foo_h(const FooOpaque * const self);";

r#"template<bool OWN_DATA>
    inline std::pair<std::string_view, std::string_view> FooWrapper<OWN_DATA>::h() const noexcept
    {

        struct CRustPairCRustStrViewCRustStrView ret = Foo_h(this->self_);
        return std::make_pair(std::string_view{ ret.first.data, ret.first.len }, std::string_view{ ret.second.data, ret.second.len });
    }"#;
