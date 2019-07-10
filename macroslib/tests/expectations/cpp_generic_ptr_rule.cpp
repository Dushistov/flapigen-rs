"static void f(const MapRect & a) noexcept;";
r#"template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::f(const MapRect & a) noexcept
    {

        Foo_f(a);
    }"#;
"void Foo_f(const MapRect * a);";

