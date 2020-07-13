"typedef void (*c_fn_i324232mut3232c_void_t)(int32_t, void *);";
r#"struct CFnOncei32 {
    c_fn_i324232mut3232c_void_t cb;
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

"typedef void (*c_fn_CRustResulti32CRustString4232mut3232c_void_t)(CRustResulti32CRustString, void *);";
"static std::future<std::variant<int32_t, RustString>> call_fn2() noexcept;";

r#"template<bool OWN_DATA>
    inline std::future<std::variant<int32_t, RustString>> TestFutureWrapper<OWN_DATA>::call_fn2() noexcept
    {

        auto tmp = new std::promise<std::variant<int32_t, RustString>>;
        auto f = tmp->get_future();
        struct CFnOnceCRustResulti32CRustString a0;
        a0.ctx = tmp;
        a0.cb = [](CRustResulti32CRustString arg, void *opaque) {
            auto arg_cpp = arg.is_ok != 0 ?
              std::variant<int32_t, RustString> { arg.data.ok } :
              std::variant<int32_t, RustString> { RustString{arg.data.err} };
            auto promise = static_cast<std::promise<std::variant<int32_t, RustString>> *>(opaque);
            promise->set_value(std::move(arg_cpp));
            delete promise;
        };

        TestFuture_call_fn2(std::move(a0));
        return f;

    }"#;
