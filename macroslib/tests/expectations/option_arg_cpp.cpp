"void f1(std::optional<double>";
"void f2(std::optional<Boo>";
"void f4(std::optional<uintptr_t> x) const noexcept;";
"static void f5(std::optional<double> x, std::optional<uintptr_t> y) noexcept;";

"void Foo_f6(struct CRustOptionCRustStrView x);";
"static void f6(std::optional<std::string_view> x) noexcept;";
r#"template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::f6(std::optional<std::string_view> x) noexcept
    {
        struct CRustOptionCRustStrView a0;
        if (!!x) {
            a0.val.data = CRustStrView{ (*x).data(), (*x).size() };
            a0.is_some = 1;
        } else {
            a0.is_some = 0;
        }
        Foo_f6(std::move(a0));
    }"#;

"void f3(std::optional<ControlItem> a0) noexcept;";
r#"template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::f3(std::optional<ControlItem> a0) noexcept
    {
        struct CRustOptionu32 a00;
        if (!!a0) {
            a00.val.data = static_cast<uint32_t>((*a0));
            a00.is_some = 1;
        } else {
            a00.is_some = 0;
        }
        Foo_f3(this->self_, std::move(a00));
    }"#;

"void Foo_f3(FooOpaque * const self, struct CRustOptionu32 a0);";

"static void f7(const Boo * x) noexcept;";
r#"template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::f7(const Boo * x) noexcept
    {

        struct CRustClassOptBoo a0 = CRustClassOptBoo { (x != nullptr) ? static_cast<BooOpaque *>(* x) : nullptr };

        Foo_f7(std::move(a0));
    }"#;

"void Foo_f7(struct CRustClassOptBoo x);";
