"std::optional<Boo> f1()";

"std::optional<double> f2()";

"std::optional<uint32_t> f3()";

"std::optional<uintptr_t> f4()";

"std::optional<BooRef> f5()";

"std::optional<ControlItem> f6() const  noexcept;";

"std::optional<uint64_t> f7()";

"std::optional<std::string_view> f8()";

r#"template<bool OWN_DATA>
    inline std::optional<std::string_view> FooWrapper<OWN_DATA>::f8() const  noexcept
    {
        struct CRustOptionStr ret = Foo_f8(this->self_);
        return ret.is_some ? std::string_view{ret.val.data, ret.val.len} : std::optional<std::string_view>();
    }"#;

"std::optional<RustString> f9()";

"std::optional<bool> f10()";

r#"template<bool OWN_DATA>
    inline std::optional<ControlItem> FooWrapper<OWN_DATA>::f6() const  noexcept
    {
        struct CRustOptionU32 ret = Foo_f6(this->self_);
        return ret.is_some ? static_cast<ControlItem>(ret.val)
 : std::optional<ControlItem>();
    }"#;

"struct CRustOptionU32 Foo_f6(const FooOpaque * const self);";
