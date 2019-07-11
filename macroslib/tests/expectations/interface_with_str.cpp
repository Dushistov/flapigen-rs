"void f1(const struct C_SomeObserver * const cb) noexcept";
"virtual void onStateChanged(std::string_view a0) = 0;";
"static void c_onStateChanged(struct CRustStrView a0, void *opaque)";
r#"struct C_SomeObserver {
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_SomeObserver_deref)(void *opaque);
    

    void (*onStateChanged)(struct CRustStrView a0, void *opaque);

};"#;
