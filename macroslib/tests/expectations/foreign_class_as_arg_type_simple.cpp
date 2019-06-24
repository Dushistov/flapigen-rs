"BooWrapper(int32_t a0, uintptr_t a1) noexcept";
r#"BooWrapper(Foo f) noexcept
    {

        this->self_ = Boo_with_foo(f.release());
        if (this->self_ == nullptr) {
            std::abort();
        }
    }"#;

"uintptr_t f(Foo foo) const";
"static int32_t f2(double a0, Foo foo) noexcept";

r#"template<bool OWN_DATA>
    inline uintptr_t BooWrapper<OWN_DATA>::f(Foo foo) const noexcept
    {

        uintptr_t ret = Boo_f(this->self_, foo.release());
        return ret;
    }"#;

r#"template<bool OWN_DATA>
    inline int32_t BooWrapper<OWN_DATA>::f2(double a0, Foo foo) noexcept
    {

        int32_t ret = Boo_f2(a0, foo.release());
        return ret;
    }"#;


"BooOpaque *Boo_with_foo(FooOpaque * f);";

"uintptr_t Boo_f(const BooOpaque * const self, FooOpaque * foo);";

"int32_t Boo_f2(double a0, FooOpaque * foo);";
