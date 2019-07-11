foreign_typemap!(
    generic_alias!(CFnOnce = swig_concat_idents!(CFnOnce, swig_i_type!(T)));
    define_c_type!(
        module = "CFnOnce!().h";
        #[repr(C)]
        struct CFnOnce!() {
            cb: extern "C" fn(swig_i_type!(T), *mut ::std::os::raw::c_void),
            ctx: *mut ::std::os::raw::c_void,
        });

    ($p:r_type) <T> impl FnOnce(T) <= CFnOnce!()
    {
        $out_no_type = |x| {
            swig_from_rust_to_i_type!(T, x, x);
            $p.cb(x, $p.ctx);
        }
    };

    ($p:f_type, input_to_output, req_modules = ["\"CFnOnce!().h\"", "<future>"]) <= "std::future<swig_f_type!(T)>"
        r#"
        auto tmp = new std::promise<swig_f_type!(T)>;
        auto $p = tmp->get_future();
        $out;
        $out.ctx = tmp;
        $out.cb = [](swig_i_type!(T) arg, void *opaque) {
            auto arg_cpp = swig_foreign_from_i_type!(T, arg);
            auto promise = static_cast<std::promise<swig_f_type!(T)> *>(opaque);
            promise->set_value(std::move(arg_cpp));
            delete promise;
        };
"#;
);

foreigner_class!(class TestFuture {
    fn call_fn(f: impl FnOnce(i32)) {
        f(5);
    }
});
