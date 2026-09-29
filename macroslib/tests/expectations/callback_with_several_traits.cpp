@@expect {"after":"\n\n};\n\n\n    t","before":"friend class TestWrapper<false>;\n\n    ","file":"Test.hpp","kind":"between"}
static void f(std::unique_ptr<MyObserver> a0) noexcept;
@@end

@@expect {"after":"\n\n} // names","before":"static void f(std::unique_ptr<MyObserver> a0) noexcept;\n\n};\n\n\n    ","file":"Test.hpp","kind":"between"}
template<bool OWN_DATA>
    inline void TestWrapper<OWN_DATA>::f(std::unique_ptr<MyObserver> a0) noexcept
    {

        C_MyObserver tmp = MyObserver::to_c_interface(std::move(a0));
        const struct C_MyObserver * const a00 = &tmp;

        Test_f(std::move(a00));
    }
@@end

@@expect {"after":"\n\n#ifdef __c","before":"extern \"C\" {\n#endif\n\n    ","file":"c_Test.h","kind":"between"}
void Test_f(const struct C_MyObserver * const a0);
@@end
