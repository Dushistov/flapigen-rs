r#"public:

    std::pair<One, Two> f() const  noexcept"#;
r#""#;

r#"template<bool OWN_DATA>
    inline std::pair<One, Two> FooWrapper<OWN_DATA>::f() const  noexcept
    {
        struct CRustObjectPair ret = Foo_f(this->self_);
        return std::make_pair(One{static_cast<OneOpaque *>(ret.first)},
 Two{static_cast<TwoOpaque *>(ret.second)});
    }"#;

"struct CRustObjectPair Foo_f(const FooOpaque * const self);";
