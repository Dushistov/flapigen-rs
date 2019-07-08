"typedef void (*c_fn_i32i32_t)(int32_t, void *);";
r#"struct CFnOncei32 {
    c_fn_i32i32_t cb;
    void * ctx;
};"#;

"static void call_fn(struct CFnOncei32 f) noexcept;";


