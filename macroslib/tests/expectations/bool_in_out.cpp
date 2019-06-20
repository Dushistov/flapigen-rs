"bool f1(bool a_0)";
r#"template<bool OWN_DATA>
    inline bool FooWrapper<OWN_DATA>::f1(bool a_0) noexcept
    {
        char ret = Foo_f1(this->self_, a_0 ? 1 : 0);
        return (ret != 0);
    }"#;
"char Foo_f1(FooOpaque * const self, char a_0);";

r#"FooWrapper(bool a_0) noexcept
    {
        this->self_ = Foo_new(a_0 ? 1 : 0);
        if (this->self_ == nullptr) {
            std::abort();
        }
    }"#;
"FooOpaque *Foo_new(char a_0);";

"static bool f2(bool a_0) noexcept;";
r#"template<bool OWN_DATA>
    inline bool FooWrapper<OWN_DATA>::f2(bool a_0) noexcept
    {
        char ret = Foo_f2(a_0 ? 1 : 0);
        return (ret != 0);
    }"#;

"virtual void onStateChanged1(int32_t a_0, bool a_1) = 0;";
r#"static void c_onStateChanged1(int32_t a_0, char a_1, void *opaque)
   {
        auto p = static_cast<SomeObserver *>(opaque);
        assert(p != nullptr);
        p->onStateChanged1(a_0, (a_1 != 0));
   }"#;
"void (*onStateChanged1)(int32_t a_0, char a_1, void *opaque);";
