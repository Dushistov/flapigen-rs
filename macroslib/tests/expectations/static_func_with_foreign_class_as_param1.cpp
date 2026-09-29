@@expect {"after":"\n\n};\n\n\n    t","before":"friend class FooWrapper<false>;\n\n    ","file":"Foo.hpp","kind":"between"}
static void static_foo(const Boo & a0) noexcept;
@@end

@@expect {"after":"\n\n} // names","before":"static void static_foo(const Boo & a0) noexcept;\n\n};\n\n\n    ","file":"Foo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::static_foo(const Boo & a0) noexcept
    {

        Foo_static_foo(static_cast<const BooOpaque *>(a0));
    }
@@end

@@expect {"after":"\n\n#ifdef __c","before":"extern \"C\" {\n#endif\n\n    ","file":"c_Foo.h","kind":"between"}
void Foo_static_foo(const BooOpaque * a0);
@@end
