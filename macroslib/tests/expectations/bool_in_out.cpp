"bool f1(bool a0) noexcept";
r#"template<bool OWN_DATA>
    inline bool FooWrapper<OWN_DATA>::f1(bool a0) noexcept
    {

        char ret = Foo_f1(this->self_, a0 ? 1 : 0);
        return (ret != 0);
    }"#;
"char Foo_f1(FooOpaque * const self, char a0);";

r#"FooWrapper(bool a0) noexcept
    {

        this->self_ = Foo_new(a0 ? 1 : 0);
        if (this->self_ == nullptr) {
            std::abort();
        }
    }"#;
"FooOpaque *Foo_new(char a0);";

"static bool f2(bool a0) noexcept;";
r#"template<bool OWN_DATA>
    inline bool FooWrapper<OWN_DATA>::f2(bool a0) noexcept
    {

        char ret = Foo_f2(a0 ? 1 : 0);
        return (ret != 0);
    }"#;

"virtual void onStateChanged1(int32_t a0, bool a1) const noexcept = 0;";
r#"static void c_onStateChanged1(int32_t a0, char a1, void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<const SomeObserver *>(opaque);

        pi->onStateChanged1(a0, (a1 != 0));
    }"#;
"void (*onStateChanged1)(int32_t a0, char a1, void *opaque);";
