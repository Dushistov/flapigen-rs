"void f1(const struct C_SomeObserver * const a_0)  noexcept";
"virtual void onStateChanged(struct RustStrView a_0) = 0;";
r#"struct C_SomeObserver {
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_SomeObserver_deref)(void *opaque);
    

    void (*onStateChanged)(struct RustStrView a_0, void *opaque);

};"#;
