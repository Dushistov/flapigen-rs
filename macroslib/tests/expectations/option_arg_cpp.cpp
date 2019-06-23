"void f1(std::optional<double>";
"void f2(std::optional<Boo>";
"void f4(std::optional<uintptr_t> x) const noexcept;";
"static void f5(std::optional<double> x, std::optional<uintptr_t> y) noexcept;";

"void Foo_f6(struct CRustOptionCRustStrView x);";
"static void f6(std::optional<std::string_view> x) noexcept;";
r#"template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::f6(std::optional<std::string_view> x) noexcept
    {

        Foo_f6(!!x ? CRustOptionCRustStrView { CRustOptionUnionCRustStrView { CRustStrView{ (*x).data(), (*x).size() } }, 1} : CRustOptionCRustStrView { {}, 0 });
    }"#;

"void f3(std::optional<ControlItem> a0) noexcept;";
r#"template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::f3(std::optional<ControlItem> a0) noexcept
    {

        Foo_f3(this->self_, !!a0 ? CRustOptionu32 { CRustOptionUnionu32 { static_cast<uint32_t>((*a0)) }, 1} : CRustOptionu32 { {}, 0 });
    }"#;

"void Foo_f3(FooOpaque * const self, struct CRustOptionu32 a0);";
