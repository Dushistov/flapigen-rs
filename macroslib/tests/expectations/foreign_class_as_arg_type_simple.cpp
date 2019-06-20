"BooWrapper(int32_t a_0, uintptr_t a_1) noexcept";
r#"BooWrapper(Foo a_0) noexcept
    {
        this->self_ = Boo_with_foo(a_0.release());
        if (this->self_ == nullptr) {
            std::abort();
        }
    }"#;

"uintptr_t f(Foo a_0) const";
"static int32_t f2(double a_0, Foo a_1) noexcept";

r#"template<bool OWN_DATA>
    inline uintptr_t BooWrapper<OWN_DATA>::f(Foo a_0) const noexcept
    {
        uintptr_t ret = Boo_f(this->self_, a_0.release());
        return ret;
    }"#;

r#"template<bool OWN_DATA>
    inline int32_t BooWrapper<OWN_DATA>::f2(double a_0, Foo a_1) noexcept
    {
        int32_t ret = Boo_f2(a_0, a_1.release());
        return ret;
    }"#;


"BooOpaque *Boo_with_foo(FooOpaque * a_0);";

"uintptr_t Boo_f(const BooOpaque * const self, FooOpaque * a_0);";

"int32_t Boo_f2(double a_0, FooOpaque * a_1);";
