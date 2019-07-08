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

    ($p:f_type, req_modules = ["\"CFnOnce!().h\""]) "CFnOnce!()";
);

foreigner_class!(class TestFuture {
    fn call_fn(f: impl FnOnce(i32)) {
        f(5);
    }
});
