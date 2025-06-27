r#"static C_Foo reference_to_c_interface(Foo &cpp_interface) noexcept
    {
        C_Foo ret;
        ret.opaque = &cpp_interface;
        ret.const_method = c_const_method;
        ret.mut_method = c_mut_method;

        ret.C_Foo_deref = [](void *) {};
        return ret;
    }"#;

"static void call_const_method(const Foo& x) noexcept;";
"static void call_mut_method(Foo& x) noexcept;";

r#"template<bool OWN_DATA>
    inline void TestFooRefWrapper<OWN_DATA>::call_const_method(const Foo& x) noexcept
    {

        C_Foo tmp = Foo::reference_to_c_interface(x);
        const struct C_Foo * const a0 = &tmp;

        TestFooRef_call_const_method(std::move(a0));
    }"#;

r#"template<bool OWN_DATA>
    inline void TestFooRefWrapper<OWN_DATA>::call_mut_method(Foo& x) noexcept
    {

        C_Foo tmp = Foo::reference_to_c_interface(x);
        struct C_Foo * const a0 = &tmp;

        TestFooRef_call_mut_method(std::move(a0));
    }"#;


"void TestFooRef_call_const_method(const struct C_Foo * const x);";
"void TestFooRef_call_mut_method(struct C_Foo * const x);";


