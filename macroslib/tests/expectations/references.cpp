@@expect {"after":"\n\n    void u","before":"            std::abort();\n        }\n    }\n\n    ","file":"TestReferences.hpp","kind":"between"}
FooRef get_foo_ref() const noexcept;
@@end

@@expect {"after":"\n\n    void u","before":"FooRef get_foo_ref() const noexcept;\n\n    ","file":"TestReferences.hpp","kind":"between"}
void update_foo(const Foo & foo) noexcept;
@@end

@@expect {"after":"\n\nprivate:\n   static voi","before":"void update_foo(const Foo & foo) noexcept;\n\n    ","file":"TestReferences.hpp","kind":"between"}
void update_mut_foo(Foo & foo) noexcept;
@@end

@@expect {"after":"\n\n    templa","before":"constexpr const uintptr_t &TestReferencesWrapper<OWN_DATA>::rust_elem_size;\n\n\n    ","file":"TestReferences.hpp","kind":"between"}
template<bool OWN_DATA>
    inline FooRef TestReferencesWrapper<OWN_DATA>::get_foo_ref() const noexcept
    {

        const FooOpaque * ret = TestReferences_get_foo_ref(this->self_);
        return FooRef{ static_cast<const FooOpaque *>(ret) };
    }
@@end

@@expect {"after":"\n\n    template<bool OWN_","before":"return FooRef{ static_cast<const FooOpaque *>(ret) };\n    }\n\n    ","file":"TestReferences.hpp","kind":"between"}
template<bool OWN_DATA>
    inline void TestReferencesWrapper<OWN_DATA>::update_foo(const Foo & foo) noexcept
    {

        TestReferences_update_foo(this->self_, static_cast<const FooOpaque *>(foo));
    }
@@end

@@expect {"after":"\n\n} // namespace org_exa","before":"TestReferences_update_foo(this->self_, static_cast<const FooOpaque *>(foo));\n    }\n\n    ","file":"TestReferences.hpp","kind":"between"}
template<bool OWN_DATA>
    inline void TestReferencesWrapper<OWN_DATA>::update_mut_foo(Foo & foo) noexcept
    {

        TestReferences_update_mut_foo(this->self_, static_cast<FooOpaque *>(foo));
    }
@@end

@@expect {"after":"\n\n    void T","before":"TestReferencesOpaque *TestReferences_new(int32_t foo_data, struct CRustStrView foo_name);\n\n    ","file":"c_TestReferences.h","kind":"between"}
const FooOpaque * TestReferences_get_foo_ref(const TestReferencesOpaque * const self);
@@end

@@expect {"after":"\n\n    void T","before":"const FooOpaque * TestReferences_get_foo_ref(const TestReferencesOpaque * const self);\n\n    ","file":"c_TestReferences.h","kind":"between"}
void TestReferences_update_foo(TestReferencesOpaque * const self, const FooOpaque * foo);
@@end

@@expect {"after":"\n\n    void T","before":"void TestReferences_update_foo(TestReferencesOpaque * const self, const FooOpaque * foo);\n\n    ","file":"c_TestReferences.h","kind":"between"}
void TestReferences_update_mut_foo(TestReferencesOpaque * const self, FooOpaque * foo);
@@end
