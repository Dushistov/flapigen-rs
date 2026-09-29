@@expect {"after":";\n\n    stati","before":"            std::abort();\n        }\n    }\n\n    ","file":"Foo.hpp","kind":"between"}
bool f1(bool a0) noexcept
@@end

@@expect {"after":"\n\n    templa","before":"constexpr const uintptr_t &FooWrapper<OWN_DATA>::rust_elem_size;\n\n\n    ","file":"Foo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline bool FooWrapper<OWN_DATA>::f1(bool a0) noexcept
    {

        char ret = Foo_f1(this->self_, static_cast<char>(a0 ? 1 : 0));
        return (ret != 0);
    }
@@end

@@expect {"after":"\n\n    char F","before":"FooOpaque *Foo_new(char a0);\n\n    ","file":"c_Foo.h","kind":"between"}
char Foo_f1(FooOpaque * const self, char a0);
@@end

@@expect {"after":"\n\n    bool f","before":"static constexpr const uintptr_t &rust_elem_size = RustForeignClassFooElemSize;\n\n    ","file":"Foo.hpp","kind":"between"}
FooWrapper(bool a0) noexcept
    {

        this->self_ = Foo_new(static_cast<char>(a0 ? 1 : 0));
        if (this->self_ == nullptr) {
            std::abort();
        }
    }
@@end

@@expect {"after":"\n\n    char F","before":"extern const uintptr_t RustForeignClassFooElemSize;\n\n    ","file":"c_Foo.h","kind":"between"}
FooOpaque *Foo_new(char a0);
@@end

@@expect {"after":"\n\nprivate:\n ","before":"bool f1(bool a0) noexcept;\n\n    ","file":"Foo.hpp","kind":"between"}
static bool f2(bool a0) noexcept;
@@end

@@expect {"after":"\n\n} // namespace org_exa","before":"return (ret != 0);\n    }\n\n    ","file":"Foo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline bool FooWrapper<OWN_DATA>::f2(bool a0) noexcept
    {

        char ret = Foo_f2(static_cast<char>(a0 ? 1 : 0));
        return (ret != 0);
    }
@@end

@@expect {"after":"\n\n\n    stati","before":"virtual ~SomeObserver() noexcept {}\n\n    ","file":"SomeObserver.hpp","kind":"between"}
virtual void onStateChanged1(int32_t a0, bool a1) const noexcept = 0;
@@end

@@expect {"after":"\n\n};\n} // na","before":"delete p;\n    }\n\n    ","file":"SomeObserver.hpp","kind":"between"}
static void c_onStateChanged1(int32_t a0, char a1, void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<const SomeObserver *>(opaque);

        pi->onStateChanged1(a0, (a1 != 0));
    }
@@end

@@expect {"after":"\n\n};\n","before":"void (*C_SomeObserver_deref)(void *opaque);\n\n    ","file":"c_SomeObserver.h","kind":"between"}
void (*onStateChanged1)(int32_t a0, char a1, void *opaque);
@@end
