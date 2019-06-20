"void f1(std::optional<double>";
"void f2(std::optional<Boo>";

"void Foo_f6(struct CRustOptionCRustStrView a_0);";
"static void f6(std::optional<std::string_view> a_0) noexcept;";
r#"template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::f6(std::optional<std::string_view> a_0) noexcept
    {
        Foo_f6(!!a_0 ? CRustOptionCRustStrView { CRustOptionUnionCRustStrView { CRustStrView{ (*a_0).data(), (*a_0).size() } }, 1} : CRustOptionCRustStrView { {}, 0 });
    }"#;

"void f3(std::optional<ControlItem> a_0) noexcept;";
r#"template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::f3(std::optional<ControlItem> a_0) noexcept
    {
        Foo_f3(this->self_, !!a_0 ? CRustOptionu32 { CRustOptionUnionu32 { static_cast<uint32_t>((*a_0)) }, 1} : CRustOptionu32 { {}, 0 });
    }"#;

"void Foo_f3(FooOpaque * const self, struct CRustOptionu32 a_0);";
