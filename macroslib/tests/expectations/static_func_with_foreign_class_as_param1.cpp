"static void static_foo(const Boo & a_0) noexcept;";
r#"template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::static_foo(const Boo & a_0) noexcept
    {
        Foo_static_foo(static_cast<const BooOpaque *>(a_0));
    }"#;
"void Foo_static_foo(const BooOpaque * a_0);";

