r#"public final void f1(@NonNull SomeObserver cb) {
        do_f1(mNativeObj, cb);
    }
    private static native void do_f1(long self, SomeObserver cb);"#;

r#"public interface SomeObserver {


    void onStateChanged(@NonNull String a0);

}"#;
