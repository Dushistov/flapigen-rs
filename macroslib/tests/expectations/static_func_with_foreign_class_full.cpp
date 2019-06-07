"static void f1(const Boo & a_0) noexcept;";
"static void f2(Boo & a_0) noexcept;";

r#"template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::f1(const Boo & a_0) noexcept
    {
        Foo_f1(static_cast<const BooOpaque *>(a_0));
    }"#;

r#"template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::f2(Boo & a_0) noexcept
    {
        Foo_f2(static_cast<BooOpaque *>(a_0));
    }"#;

"void Foo_f2(BooOpaque * a_0);";
