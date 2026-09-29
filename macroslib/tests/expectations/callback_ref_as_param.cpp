@@expect {"after":"\n    static C_Foo refere","before":"  return ret;\n    }\n    ","file":"Foo.hpp","kind":"between"}
static C_Foo reference_to_c_interface(Foo &cpp_interface) noexcept
    {
        C_Foo ret;
        ret.opaque = &cpp_interface;
        ret.const_method = c_const_method;
        ret.mut_method = c_mut_method;

        ret.C_Foo_deref = [](void *) {};
        return ret;
    }
@@end

@@expect {"after":"\n\n    static","before":"friend class TestFooRefWrapper<false>;\n\n    ","file":"TestFooRef.hpp","kind":"between"}
static void call_const_method(const Foo& x) noexcept;
@@end

@@expect {"after":"\n\n};\n\n\n    t","before":"static void call_const_method(const Foo& x) noexcept;\n\n    ","file":"TestFooRef.hpp","kind":"between"}
static void call_mut_method(Foo& x) noexcept;
@@end

@@expect {"after":"\n\n    templa","before":"static void call_mut_method(Foo& x) noexcept;\n\n};\n\n\n    ","file":"TestFooRef.hpp","kind":"between"}
template<bool OWN_DATA>
    inline void TestFooRefWrapper<OWN_DATA>::call_const_method(const Foo& x) noexcept
    {

        C_Foo tmp = Foo::reference_to_c_interface(x);
        const struct C_Foo * const a0 = &tmp;

        TestFooRef_call_const_method(std::move(a0));
    }
@@end

@@expect {"after":"\n\n} // names","before":"TestFooRef_call_const_method(std::move(a0));\n    }\n\n    ","file":"TestFooRef.hpp","kind":"between"}
template<bool OWN_DATA>
    inline void TestFooRefWrapper<OWN_DATA>::call_mut_method(Foo& x) noexcept
    {

        C_Foo tmp = Foo::reference_to_c_interface(x);
        struct C_Foo * const a0 = &tmp;

        TestFooRef_call_mut_method(std::move(a0));
    }
@@end

@@expect {"after":"\n\n    void T","before":"extern \"C\" {\n#endif\n\n    ","file":"c_TestFooRef.h","kind":"between"}
void TestFooRef_call_const_method(const struct C_Foo * const x);
@@end

@@expect {"after":"\n\n#ifdef __c","before":"void TestFooRef_call_const_method(const struct C_Foo * const x);\n\n    ","file":"c_TestFooRef.h","kind":"between"}
void TestFooRef_call_mut_method(struct C_Foo * const x);
@@end
