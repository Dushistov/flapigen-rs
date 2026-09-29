@@expect {"after":"\n\n};\n\n\n    t","before":"friend class FooWrapper<false>;\n\n    ","file":"Foo.hpp","kind":"between"}
static void f(const MapRect & a) noexcept;
@@end

@@expect {"after":"\n\n} // names","before":"static void f(const MapRect & a) noexcept;\n\n};\n\n\n    ","file":"Foo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::f(const MapRect & a) noexcept
    {

        Foo_f(a);
    }
@@end

@@expect {"after":"\n\n#ifdef __c","before":"extern \"C\" {\n#endif\n\n    ","file":"c_Foo.h","kind":"between"}
void Foo_f(const MapRect * a);
@@end
