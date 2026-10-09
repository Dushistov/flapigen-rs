@@expect {"file":"Foo.hpp","kind":"item","name":"static_foo","form":"declaration"}
static void static_foo(const Boo & a0) noexcept;
@@end

@@expect {"file":"Foo.hpp","kind":"between","before":"namespace org_examples {\n\n\n    ","after":"\n\n} // namespace org_examples"}
template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::static_foo(const Boo & a0) noexcept
    {

        Foo_static_foo(static_cast<const BooOpaque *>(a0));
    }
@@end

@@expect {"after":"\n\n#ifdef __c","before":"extern \"C\" {\n#endif\n\n    ","file":"c_Foo.h","kind":"between"}
void Foo_static_foo(const BooOpaque * a0);
@@end
