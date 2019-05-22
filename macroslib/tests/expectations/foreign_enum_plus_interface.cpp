"virtual void onSessionUpdate(ControlItem a_0, bool a_1) = 0;";

r#"enum ControlItem {
GNSS = 0,
GPS_PROVIDER = 1

};"#;


r#"struct C_ControlStateObserver {
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_ControlStateObserver_deref)(void *opaque);
    

    void (*onSessionUpdate)(uint32_t a_0, char a_1, void *opaque);

};"#;

r#"static void c_onSessionUpdate(uint32_t a_0, char a_1, void *opaque)
   {
        auto p = static_cast<ControlStateObserver *>(opaque);
        assert(p != nullptr);
        p->onSessionUpdate(static_cast<ControlItem>(a_0), a_1 != 0);
   }"#;
