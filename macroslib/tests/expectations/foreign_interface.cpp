"virtual void onStateChanged(int32_t a_0, bool a_1) = 0;";
"virtual void onStateChangedWithoutArgs() = 0;";
r#"struct C_SomeObserver {
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_SomeObserver_deref)(void *opaque);
    

    void (*onStateChanged)(int32_t a_0, char a_1, void *opaque);


    void (*onStateChangedWithoutArgs)(void *opaque);

};"#;

"void f1(const struct C_SomeObserver * const a_0) noexcept";

r#"template<bool OWN_DATA>
    inline void ClassWithCallbacksWrapper<OWN_DATA>::f1(const struct C_SomeObserver * const a_0) noexcept
    {
        ClassWithCallbacks_f1(this->self_, a_0);
    }"#;

"void ClassWithCallbacks_f1(ClassWithCallbacksOpaque * const self, const struct C_SomeObserver * const a_0);";
