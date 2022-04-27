r#"class SomeObserver {
public:
    virtual ~SomeObserver() noexcept {}

    virtual void onStateChanged(int32_t a0, bool a1) const noexcept = 0;

    virtual void onStateChangedWithoutArgs() const noexcept = 0;

    virtual void onStateChangedFoo(Foo foo) const noexcept = 0;

    virtual float getTextSize() const noexcept = 0;
"#;

r#"    static void c_SomeObserver_deref(void *opaque)
    {
        auto p = static_cast<SomeObserver *>(opaque);
        delete p;
    }

    static void c_onStateChanged(int32_t a0, char a1, void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<const SomeObserver *>(opaque);

        pi->onStateChanged(a0, (a1 != 0));
    }

    static void c_onStateChangedWithoutArgs(void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<const SomeObserver *>(opaque);

        pi->onStateChangedWithoutArgs();
    }

    static void c_onStateChangedFoo(FooOpaque * foo, void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<const SomeObserver *>(opaque);

        pi->onStateChangedFoo(Foo(static_cast<FooOpaque *>(foo)));
    }

    static float c_getTextSize(void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<const SomeObserver *>(opaque);

        auto ret = pi->getTextSize();
        return ret;
    }"#;

r#"struct C_SomeObserver {
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_SomeObserver_deref)(void *opaque);

    void (*onStateChanged)(int32_t a0, char a1, void *opaque);

    void (*onStateChangedWithoutArgs)(void *opaque);

    void (*onStateChangedFoo)(FooOpaque * foo, void *opaque);

    float (*getTextSize)(void *opaque);

};"#;

"void f1(std::unique_ptr<SomeObserver> cb) noexcept;";

r#"template<bool OWN_DATA>
    inline void ClassWithCallbacksWrapper<OWN_DATA>::f1(std::unique_ptr<SomeObserver> cb) noexcept
    {

        C_SomeObserver tmp = SomeObserver::to_c_interface(std::move(cb));
        const struct C_SomeObserver * const a0 = &tmp;

        ClassWithCallbacks_f1(this->self_, std::move(a0));
    }"#;

"void ClassWithCallbacks_f1(ClassWithCallbacksOpaque * const self, const struct C_SomeObserver * const cb);";


