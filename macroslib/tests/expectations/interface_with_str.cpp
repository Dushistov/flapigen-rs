"void f1(const struct C_SomeObserver * const a_0) noexcept";
"virtual void onStateChanged(std::string_view a_0) = 0;";
"static void c_onStateChanged(struct CRustStrView a_0, void *opaque)";
r#"struct C_SomeObserver {
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_SomeObserver_deref)(void *opaque);
    

    void (*onStateChanged)(struct CRustStrView a_0, void *opaque);

};"#;
