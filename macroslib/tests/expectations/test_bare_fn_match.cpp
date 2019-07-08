"typedef void (*c_fn_i32i32_t)(int32_t, void *);";
r#"struct CFnOncei32 {
    c_fn_i32i32_t cb;
    void * ctx;
};"#;

"static std::future<int32_t> call_fn() noexcept;";
r#"template<bool OWN_DATA>
    inline std::future<int32_t> TestFutureWrapper<OWN_DATA>::call_fn() noexcept
    {

        auto tmp = new std::promise<int32_t>;
        auto f = tmp->get_future();
        struct CFnOncei32 a0;
        a0.ctx = tmp;
        a0.cb = [](int32_t arg, void *opaque) {
            auto arg_cpp = arg;
            auto promise = static_cast<std::promise<int32_t> *>(opaque);
            promise->set_value(std::move(arg_cpp));
            delete promise;
        };

        TestFuture_call_fn(std::move(a0));
        return f;

    }"#;


