@@expect {"after":"\n    {\n\n    ","before":"static constexpr const uintptr_t &rust_elem_size = RustForeignClassBooElemSize;\n\n    ","file":"Boo.hpp","kind":"between"}
BooWrapper(int32_t a0, uintptr_t a1) noexcept
@@end

@@expect {"after":"\n\n    uintpt","before":"        this->self_ = Boo_new(a0, a1);\n        if (this->self_ == nullptr) {\n            std::abort();\n        }\n    }\n\n    ","file":"Boo.hpp","kind":"between"}
BooWrapper(Foo f) noexcept
    {

        this->self_ = Boo_with_foo(f.release());
        if (this->self_ == nullptr) {
            std::abort();
        }
    }
@@end

@@expect {"after":" noexcept;\n\n    static int32_t f2(double a0, Foo foo) noexcept;\n\nprivate:\n   static void free_mem(SelfType &p) noexcept\n   {\n        if (OWN_DATA && p != nullpt","before":"BooWrapper(Foo f) noexcept\n    {\n\n        this->self_ = Boo_with_foo(f.release());\n        if (this->self_ == nullptr) {\n            std::abort();\n        }\n    }\n\n    ","file":"Boo.hpp","kind":"between"}
uintptr_t f(Foo foo) const
@@end

@@expect {"after":";\n\nprivate:\n","before":"uintptr_t f(Foo foo) const noexcept;\n\n    ","file":"Boo.hpp","kind":"between"}
static int32_t f2(double a0, Foo foo) noexcept
@@end

@@expect {"after":"\n\n    templa","before":"constexpr const uintptr_t &BooWrapper<OWN_DATA>::rust_elem_size;\n\n\n    ","file":"Boo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline uintptr_t BooWrapper<OWN_DATA>::f(Foo foo) const noexcept
    {

        uintptr_t ret = Boo_f(this->self_, foo.release());
        return ret;
    }
@@end

@@expect {"after":"\n\n} // namespace org_exa","before":" return ret;\n    }\n\n    ","file":"Boo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline int32_t BooWrapper<OWN_DATA>::f2(double a0, Foo foo) noexcept
    {

        int32_t ret = Boo_f2(a0, foo.release());
        return ret;
    }
@@end

@@expect {"after":"\n\n    uintpt","before":"BooOpaque *Boo_new(int32_t a0, uintptr_t a1);\n\n    ","file":"c_Boo.h","kind":"between"}
BooOpaque *Boo_with_foo(FooOpaque * f);
@@end

@@expect {"after":"\n\n    int32_","before":"BooOpaque *Boo_with_foo(FooOpaque * f);\n\n    ","file":"c_Boo.h","kind":"between"}
uintptr_t Boo_f(const BooOpaque * const self, FooOpaque * foo);
@@end

@@expect {"after":"\n\n    void B","before":"uintptr_t Boo_f(const BooOpaque * const self, FooOpaque * foo);\n\n    ","file":"c_Boo.h","kind":"between"}
int32_t Boo_f2(double a0, FooOpaque * foo);
@@end
