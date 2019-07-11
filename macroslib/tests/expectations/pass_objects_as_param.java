"public final void f1(long a0)";
r#"    public final void f2(@NonNull Foo a0) {

        long a00 = a0.mNativeObj;
        a0.mNativeObj = 0;

        do_f2(mNativeObj, a00);
    }"#;
"public final void f3(long a0)";
