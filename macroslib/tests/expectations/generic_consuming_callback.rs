trait AsyncCallbacks<T> {
    fn is_cancelled(&self) -> bool;
    fn on_result_ready(self, result: T);
}

foreign_callback!(callback Completion<T> {
    self_type AsyncCallbacks<T> + Send;
    isCancelled = AsyncCallbacks::is_cancelled(&self) -> bool;
    onResultReady = AsyncCallbacks::on_result_ready(self, result: T);
});

foreign_typemap!(
    ($p:r_type) <T> impl AsyncCallbacks<T> <= swig_callback_i_type!(Completion, T) {
        $out_no_type = $p;
    };

    ($p:f_type, $tmp:temporary, input_to_output)
        <= "CancelableFuture<swig_f_type!(T, output)>"
        r#"
        auto $tmp = new CancelableFutureState<swig_f_type!(T, output)>;
        auto $p = CancelableFuture<swig_f_type!(T, output)>($tmp);
        $out;
        $out.opaque = $tmp;
        $out.swig_callback_i_type!(Completion, T)_deref = [](void *opaque) {};
        $out.isCancelled = [](void *opaque) -> char { return 0; };
        $out.onResultReady = [](swig_i_type!(T, output), void *opaque) {};
"#;
);

foreign_class!(class Test {
    fn run_i32(callback: impl AsyncCallbacks<i32>);
    fn run_string(callback: impl AsyncCallbacks<String>);
});
