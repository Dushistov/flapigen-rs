@@expect {"file":"Foo.hpp","kind":"between","before":"explicit FooWrapper(SelfType o) noexcept: self_(o) {}\n    ","after":"\n    FooOpaque *release() noexcept"}
template<bool B = OWN_DATA, typename std::enable_if<!B, int>::type = 0>
    FooWrapper(const FooWrapper<true> &o) noexcept: self_(o.self_) {}
@@end

@@expect {"file":"Foo.hpp","kind":"between","before":"explicit operator SelfType() const noexcept { return self_; }\n\n    ","after":"\n    static constexpr const uintptr_t &rust_elem_size"}
FooWrapper(const FooWrapper&) = default;
    FooWrapper &operator=(const FooWrapper&) = default;
@@end

@@expect {"after":"\n\n    FooRef get_mut_foo_ref() noexcept;","before":"            std::abort();\n        }\n    }\n\n    ","file":"TestReferences.hpp","kind":"between"}
FooRef get_foo_ref() const noexcept;
@@end

@@expect {"after":"\n\n    void update_foo","before":"FooRef get_foo_ref() const noexcept;\n\n    ","file":"TestReferences.hpp","kind":"between"}
FooRef get_mut_foo_ref() noexcept;
@@end

@@expect {"after":"\n\n    void update_mut_foo","before":"FooRef get_mut_foo_ref() noexcept;\n\n    ","file":"TestReferences.hpp","kind":"between"}
void update_foo(FooRef foo) noexcept;
@@end

@@expect {"after":"\n\nprivate:\n   static voi","before":"void update_foo(FooRef foo) noexcept;\n\n    ","file":"TestReferences.hpp","kind":"between"}
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

@@expect {"file":"TestReferences.hpp","kind":"item","name":"TestReferencesWrapper<OWN_DATA>::get_mut_foo_ref","form":"definition"}
template<bool OWN_DATA>
    inline FooRef TestReferencesWrapper<OWN_DATA>::get_mut_foo_ref() noexcept
    {

        const FooOpaque * ret = TestReferences_get_mut_foo_ref(this->self_);
        return FooRef{ static_cast<const FooOpaque *>(ret) };
    }
@@end

@@expect {"after":"\n\n    template<bool OWN_","before":"inline FooRef TestReferencesWrapper<OWN_DATA>::get_mut_foo_ref() noexcept\n    {\n\n        const FooOpaque * ret = TestReferences_get_mut_foo_ref(this->self_);\n        return FooRef{ static_cast<const FooOpaque *>(ret) };\n    }\n\n    ","file":"TestReferences.hpp","kind":"between"}
template<bool OWN_DATA>
    inline void TestReferencesWrapper<OWN_DATA>::update_foo(FooRef foo) noexcept
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

@@expect {"after":"\n\n    const FooOpaque * TestReferences_get_mut_foo_ref","before":"TestReferencesOpaque *TestReferences_new(int32_t foo_data, struct CRustStrView foo_name);\n\n    ","file":"c_TestReferences.h","kind":"between"}
const FooOpaque * TestReferences_get_foo_ref(const TestReferencesOpaque * const self);
@@end

@@expect {"after":"\n\n    void T","before":"const FooOpaque * TestReferences_get_foo_ref(const TestReferencesOpaque * const self);\n\n    ","file":"c_TestReferences.h","kind":"between"}
const FooOpaque * TestReferences_get_mut_foo_ref(TestReferencesOpaque * const self);
@@end

@@expect {"after":"\n\n    void T","before":"const FooOpaque * TestReferences_get_mut_foo_ref(TestReferencesOpaque * const self);\n\n    ","file":"c_TestReferences.h","kind":"between"}
void TestReferences_update_foo(TestReferencesOpaque * const self, const FooOpaque * foo);
@@end

@@expect {"after":"\n\n    void T","before":"void TestReferences_update_foo(TestReferencesOpaque * const self, const FooOpaque * foo);\n\n    ","file":"c_TestReferences.h","kind":"between"}
void TestReferences_update_mut_foo(TestReferencesOpaque * const self, FooOpaque * foo);
@@end
