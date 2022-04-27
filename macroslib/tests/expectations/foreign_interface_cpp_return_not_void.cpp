"virtual bool onStateChanged(int32_t a0, bool a1) const noexcept = 0;";
"virtual void onStateChangedWithoutArgs() const noexcept = 0;";
r#"struct C_SomeObserver {
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_SomeObserver_deref)(void *opaque);

    char (*onStateChanged)(int32_t a0, char a1, void *opaque);

    void (*onStateChangedWithoutArgs)(void *opaque);

};"#;
