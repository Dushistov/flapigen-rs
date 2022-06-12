"virtual void onSessionUpdate(ControlItem item, bool is_ok) const noexcept = 0;";

r#"enum ControlItem {
GNSS = 0,
GPS_PROVIDER = 1
};"#;


r#"struct C_ControlStateObserver {
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_ControlStateObserver_deref)(void *opaque);

    void (*onSessionUpdate)(uint32_t item, char is_ok, void *opaque);

};"#;

r#"static void c_onSessionUpdate(uint32_t item, char is_ok, void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<const ControlStateObserver *>(opaque);

        pi->onSessionUpdate(static_cast<ControlItem>(item), (is_ok != 0));
    }"#;
