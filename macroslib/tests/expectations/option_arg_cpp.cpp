"void f1(std::optional<double>";
"void f2(std::optional<Boo>";
"void f3(std::optional<ControlItem>";
"void Foo_f6(const char * a_0);";
"static void f6(std::optional<const char *> a_0) noexcept;";
r#"inline void FooWrapper<OWN_DATA>::f6(std::optional<const char *> a_0) noexcept
    {
        Foo_f6(!!a_0 ? *a_0 : nullptr);
    }"#;
