"static void f1(const Boo & a0) noexcept;";
"static void f2(Boo & a0) noexcept;";

r#"template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::f1(const Boo & a0) noexcept
    {

        Foo_f1(static_cast<const BooOpaque *>(a0));
    }"#;

r#"template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::f2(Boo & a0) noexcept
    {

        Foo_f2(static_cast<BooOpaque *>(a0));
    }"#;

"void Foo_f2(BooOpaque * a0);";
