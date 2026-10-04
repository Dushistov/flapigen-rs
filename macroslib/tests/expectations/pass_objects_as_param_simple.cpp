@@expect {"after":"\n    extern ","before":"extern \"C\" {\n#endif\n\n\n    ","file":"c_Foo.h","kind":"between"}
typedef struct FooOpaque FooOpaque;
@@end

@@expect {"after":"\n\n    void T","before":"TestPassObjectsAsParamsOpaque *TestPassObjectsAsParams_default();\n\n    ","file":"c_TestPassObjectsAsParams.h","kind":"between"}
void TestPassObjectsAsParams_f1(const TestPassObjectsAsParamsOpaque * const self, const FooOpaque * a0);
@@end

@@expect {"after":"\n\n    void TestPassObjectsAsParams_f3(co","before":"void TestPassObjectsAsParams_f1(const TestPassObjectsAsParamsOpaque * const self, const FooOpaque * a0);\n\n    ","file":"c_TestPassObjectsAsParams.h","kind":"between"}
void TestPassObjectsAsParams_f2(const TestPassObjectsAsParamsOpaque * const self, FooOpaque * a0);
@@end

@@expect {"after":"\n\n    void TestPassObjectsAsParams_f3_a(","before":"void TestPassObjectsAsParams_f2(const TestPassObjectsAsParamsOpaque * const self, FooOpaque * a0);\n\n    ","file":"c_TestPassObjectsAsParams.h","kind":"between"}
void TestPassObjectsAsParams_f3(const TestPassObjectsAsParamsOpaque * const self, FooOpaque * a0);
@@end

@@expect {"after":"\n\n    void TestPassObjectsAsParams_f4(const FooOpaque * a0);\n\n    void TestPassO","before":"void TestPassObjectsAsParams_f3(const TestPassObjectsAsParamsOpaque * const self, FooOpaque * a0);\n\n    ","file":"c_TestPassObjectsAsParams.h","kind":"between"}
void TestPassObjectsAsParams_f3_a(const TestPassObjectsAsParamsOpaque * const self, BooOpaque * a0);
@@end

@@expect {"after":"\n\n    void TestPassObjec","before":"void TestPassObjectsAsParams_f3_a(const TestPassObjectsAsParamsOpaque * const self, BooOpaque * a0);\n\n    ","file":"c_TestPassObjectsAsParams.h","kind":"between"}
void TestPassObjectsAsParams_f4(const FooOpaque * a0);
@@end

@@expect {"after":"\n\n    void TestPassObjectsAsParams_delet","before":"void TestPassObjectsAsParams_f4(const FooOpaque * a0);\n\n    ","file":"c_TestPassObjectsAsParams.h","kind":"between"}
void TestPassObjectsAsParams_f5(FooOpaque * a0);
@@end

@@expect {"after":";\n\n    void ","before":"            std::abort();\n        }\n    }\n\n    ","file":"TestPassObjectsAsParams.hpp","kind":"between"}
void f1(FooRef a0) const noexcept
@@end

@@expect {"after":";\n\n    void f3(Foo & a0)","before":"void f1(FooRef a0) const noexcept;\n\n    ","file":"TestPassObjectsAsParams.hpp","kind":"between"}
void f2(Foo a0) const noexcept
@@end

@@expect {"after":";\n\n    void f3_a(Boo & a0) const noexcep","before":"   void f2(Foo a0) const noexcept;\n\n    ","file":"TestPassObjectsAsParams.hpp","kind":"between"}
void f3(Foo & a0) const noexcept
@@end

@@expect {"after":";\n\n    static void f4(FooRef a0) no","before":" void f3(Foo & a0) const noexcept;\n\n    ","file":"TestPassObjectsAsParams.hpp","kind":"between"}
void f3_a(Boo & a0) const noexcept
@@end

@@expect {"after":";\n\n    static void f5(Foo a0) noexcept;\n","before":"void f3_a(Boo & a0) const noexcept;\n\n    ","file":"TestPassObjectsAsParams.hpp","kind":"between"}
static void f4(FooRef a0) noexcept
@@end

@@expect {"after":";\n\nprivate:\n   static vo","before":"static void f4(FooRef a0) noexcept;\n\n    ","file":"TestPassObjectsAsParams.hpp","kind":"between"}
static void f5(Foo a0) noexcept
@@end

@@expect {"after":"\n\n    template<bool OWN_DATA>\n    inline","before":"constexpr const uintptr_t &TestPassObjectsAsParamsWrapper<OWN_DATA>::rust_elem_size;\n\n\n    template<bool OWN_DATA>\n    ","file":"TestPassObjectsAsParams.hpp","kind":"between"}
inline void TestPassObjectsAsParamsWrapper<OWN_DATA>::f1(FooRef a0) const noexcept
    {

        TestPassObjectsAsParams_f1(this->self_, static_cast<const FooOpaque *>(a0));
    }
@@end

@@expect {"after":"\n\n    template<bool OWN_DATA>\n    inline void TestPassObjectsAsParamsWrapper<OWN","before":"TestPassObjectsAsParams_f1(this->self_, static_cast<const FooOpaque *>(a0));\n    }\n\n    template<bool OWN_DATA>\n    ","file":"TestPassObjectsAsParams.hpp","kind":"between"}
inline void TestPassObjectsAsParamsWrapper<OWN_DATA>::f2(Foo a0) const noexcept
    {

        TestPassObjectsAsParams_f2(this->self_, a0.release());
    }
@@end

@@expect {"after":"\n\n    template<bool OWN_DATA>\n    inline void TestPassObjectsAsParamsWrapper<OWN","before":"TestPassObjectsAsParams_f2(this->self_, a0.release());\n    }\n\n    template<bool OWN_DATA>\n    ","file":"TestPassObjectsAsParams.hpp","kind":"between"}
inline void TestPassObjectsAsParamsWrapper<OWN_DATA>::f3(Foo & a0) const noexcept
    {

        TestPassObjectsAsParams_f3(this->self_, static_cast<FooOpaque *>(a0));
    }
@@end

@@expect {"after":"\n\n    template<bool OWN_DATA>\n    inline","before":"TestPassObjectsAsParams_f3(this->self_, static_cast<FooOpaque *>(a0));\n    }\n\n    ","file":"TestPassObjectsAsParams.hpp","kind":"between"}
template<bool OWN_DATA>
    inline void TestPassObjectsAsParamsWrapper<OWN_DATA>::f3_a(Boo & a0) const noexcept
    {

        TestPassObjectsAsParams_f3_a(this->self_, static_cast<BooOpaque *>(a0));
    }
@@end

@@expect {"after":"\n\n    template<bool OWN_DATA>\n    inline","before":"TestPassObjectsAsParams_f3_a(this->self_, static_cast<BooOpaque *>(a0));\n    }\n\n    ","file":"TestPassObjectsAsParams.hpp","kind":"between"}
template<bool OWN_DATA>
    inline void TestPassObjectsAsParamsWrapper<OWN_DATA>::f4(FooRef a0) noexcept
    {

        TestPassObjectsAsParams_f4(static_cast<const FooOpaque *>(a0));
    }
@@end

@@expect {"after":"\n\n} // namespace org_examples\n","before":"     TestPassObjectsAsParams_f4(static_cast<const FooOpaque *>(a0));\n    }\n\n    ","file":"TestPassObjectsAsParams.hpp","kind":"between"}
template<bool OWN_DATA>
    inline void TestPassObjectsAsParamsWrapper<OWN_DATA>::f5(Foo a0) noexcept
    {

        TestPassObjectsAsParams_f5(a0.release());
    }
@@end
