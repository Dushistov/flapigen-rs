@@expect {"after":"\n\n    static","before":"friend class FooWrapper<false>;\n\n    ","file":"Foo.hpp","kind":"between"}
static void f1(const Boo & a0) noexcept;
@@end

@@expect {"after":"\n\n};\n\n\n    t","before":"static void f1(const Boo & a0) noexcept;\n\n    ","file":"Foo.hpp","kind":"between"}
static void f2(Boo & a0) noexcept;
@@end

@@expect {"after":"\n\n    templa","before":"static void f2(Boo & a0) noexcept;\n\n};\n\n\n    ","file":"Foo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::f1(const Boo & a0) noexcept
    {

        Foo_f1(static_cast<const BooOpaque *>(a0));
    }
@@end

@@expect {"after":"\n\n} // names","before":"Foo_f1(static_cast<const BooOpaque *>(a0));\n    }\n\n    ","file":"Foo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::f2(Boo & a0) noexcept
    {

        Foo_f2(static_cast<BooOpaque *>(a0));
    }
@@end

@@expect {"after":"\n\n#ifdef __c","before":"void Foo_f1(const BooOpaque * a0);\n\n    ","file":"c_Foo.h","kind":"between"}
void Foo_f2(BooOpaque * a0);
@@end
