"void f1(std::optional<double>";
"void f2(std::optional<Boo>";
"void f3(std::optional<ControlItem> a_0)  noexcept;";
"void Foo_f6(const char * a_0);";
"static void f6(std::optional<const char *> a_0) noexcept;";
r#"inline void FooWrapper<OWN_DATA>::f6(std::optional<const char *> a_0) noexcept
    {
        Foo_f6(!!a_0 ? *a_0 : nullptr);
    }"#;
r#"template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::f3(std::optional<ControlItem> a_0)  noexcept
    {
        Foo_f3(this->self_, !!a_0 ? CRustOptionU32{static_cast<uint32_t>(*a_0), 1} : c_option_empty<CRustOptionU32>());
    }"#;

"void Foo_f3(FooOpaque * const self, struct CRustOptionU32 a_0);";
