r#"class Completion {
public:
    virtual ~Completion() noexcept {}

    virtual bool isCancelled() const noexcept = 0;

    virtual void onResultReady(int32_t result) noexcept = 0;"#;

r#"struct C_Completion {
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_Completion_deref)(void *opaque);

    char (*isCancelled)(void *opaque);

    void (*onResultReady)(int32_t result, void *opaque);

};"#;

r#"static char c_isCancelled(void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<const Completion *>(opaque);

        auto ret = pi->isCancelled();
        return static_cast<char>(ret ? 1 : 0);
    }"#;
    
r#"static void c_onResultReady(int32_t result, void *opaque)
    {
        assert(opaque != nullptr);
        auto pi = static_cast<Completion *>(opaque);

        pi->onResultReady(result);
        delete pi;
    }"#;

r#"static void c_Completion_deref(void *opaque)
    {
        auto p = static_cast<Completion *>(opaque);
        delete p;
    }"#;
