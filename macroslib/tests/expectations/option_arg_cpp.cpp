"void f1(std::optional<double>";
"void f2(std::optional<Boo>";
"void f4(std::optional<uintptr_t> x) const noexcept;";
"static void f5(std::optional<double> x, std::optional<uintptr_t> y) noexcept;";

"void Foo_f6(struct CRustOptionCRustStrView x);";
"static void f6(std::optional<std::string_view> x) noexcept;";
r#"template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::f6(std::optional<std::string_view> x) noexcept
    {

        Foo_f6([](std::optional<std::string_view> p) -> CRustOptionCRustStrView {
            CRustOptionCRustStrView out;
            if (p.has_value()) {
                out.val.data = CRustStrView{ (*p).data(), (*p).size() };
                out.is_some = 1;
            } else {
                out.is_some = 0;
            }
            return out;
            }(std::move(x)));
    }"#;

"void f3(std::optional<ControlItem> a0) noexcept;";
r#"template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::f3(std::optional<ControlItem> a0) noexcept
    {

        Foo_f3(this->self_, [](std::optional<ControlItem> p) -> CRustOptionu32 {
            CRustOptionu32 out;
            if (p.has_value()) {
                out.val.data = static_cast<uint32_t>((*p));
                out.is_some = 1;
            } else {
                out.is_some = 0;
            }
            return out;
            }(std::move(a0)));
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
