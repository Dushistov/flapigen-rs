"FooRef get_foo_ref() const  noexcept;";
"void update_foo(const Foo & a_0)  noexcept;";
"void update_mut_foo(Foo & a_0)  noexcept;";

r#"template<bool OWN_DATA>
    inline FooRef TestReferencesWrapper<OWN_DATA>::get_foo_ref() const  noexcept
    {
        const FooOpaque * ret = TestReferences_get_foo_ref(this->self_);
        return FooRef{ static_cast<const FooOpaque *>(ret) };
    }"#;

r#"template<bool OWN_DATA>
    inline void TestReferencesWrapper<OWN_DATA>::update_foo(const Foo & a_0)  noexcept
    {
        TestReferences_update_foo(this->self_, static_cast<const FooOpaque *>(a_0));
    }"#;

r#"template<bool OWN_DATA>
    inline void TestReferencesWrapper<OWN_DATA>::update_mut_foo(Foo & a_0)  noexcept
    {
        TestReferences_update_mut_foo(this->self_, static_cast<FooOpaque *>(a_0));
    }"#;

"const FooOpaque * TestReferences_get_foo_ref(const TestReferencesOpaque * const self);";
"void TestReferences_update_foo(TestReferencesOpaque * const self, const FooOpaque * a_0);";
"void TestReferences_update_mut_foo(TestReferencesOpaque * const self, FooOpaque * a_0);";
