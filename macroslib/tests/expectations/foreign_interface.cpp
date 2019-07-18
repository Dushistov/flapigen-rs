"virtual void onStateChanged(int32_t a0, bool a1) = 0;";
"virtual void onStateChangedWithoutArgs() = 0;";
"virtual void onStateChangedFoo(Foo foo) = 0;";
r#"struct C_SomeObserver {
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_SomeObserver_deref)(void *opaque);

    void (*onStateChanged)(int32_t a0, char a1, void *opaque);

    void (*onStateChangedWithoutArgs)(void *opaque);

    void (*onStateChangedFoo)(FooOpaque * foo, void *opaque);

};"#;

"void f1(const struct C_SomeObserver * const cb) noexcept";
r#"template<bool OWN_DATA>
    inline void ClassWithCallbacksWrapper<OWN_DATA>::f1(const struct C_SomeObserver * const cb) noexcept
    {

        ClassWithCallbacks_f1(this->self_, cb);
    }"#;
"void ClassWithCallbacks_f1(ClassWithCallbacksOpaque * const self, const struct C_SomeObserver * const cb);";
