r#"class Completioni32 {
public:
    virtual ~Completioni32() noexcept {}

    virtual bool isCancelled() const noexcept = 0;

    virtual void onResultReady(int32_t result) noexcept = 0;"#;

r#"class CompletionCRustString {
public:
    virtual ~CompletionCRustString() noexcept {}

    virtual bool isCancelled() const noexcept = 0;

    virtual void onResultReady(RustString result) noexcept = 0;"#;

r#"struct C_Completioni32 {
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_Completioni32_deref)(void *opaque);

    char (*isCancelled)(void *opaque);

    void (*onResultReady)(int32_t result, void *opaque);

};"#;

r#"struct C_CompletionCRustString {
    void *opaque;
    //! call by Rust side when callback not need anymore
    void (*C_CompletionCRustString_deref)(void *opaque);

    char (*isCancelled)(void *opaque);

    void (*onResultReady)(struct CRustString result, void *opaque);

};"#;
