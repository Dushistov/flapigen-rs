@@expect {"after":"\n\nprivate:\n   static void free_mem(SelfType &p) noexcept\n   {\n        if (OWN_DATA && p != nullptr) {\n            Foo_delete(p);\n        }\n        p = nullptr;\n","before":"static constexpr const uintptr_t &rust_elem_size = RustForeignClassFooElemSize;\n\n    FooWrapper() noexcept\n    {\n\n        this->self_ = Foo_new();\n        if (this->self_ == nullptr) {\n            std::abort();\n        }\n    }\n","file":"Foo.hpp","kind":"between"}
private:

    FooWrapper(int32_t a0) noexcept
    {

        this->self_ = private_Foo_from_int(a0);
        if (this->self_ == nullptr) {
            std::abort();
        }
    }

    static void private_f() noexcept;
public:

    static void public_f() noexcept;
protected:

    static void protected_f() noexcept;
@@end
