"public Foo(int a0)";
"public final int f(int a0, int a1)";
"public Boo(int a0, long a1)";
r#"public Boo(@NonNull Foo f) {

        long a0 = f.mNativeObj;
        f.mNativeObj = 0;

        mNativeObj = init(a0);
    }
    private static native long init(long f);"#;
r#"public final long f(@NonNull Foo foo) {

        long a0 = foo.mNativeObj;
        foo.mNativeObj = 0;

        return do_f(mNativeObj, a0);
    }"#;
r#"public static int f2(double a0, @NonNull Foo foo) {

        long a1 = foo.mNativeObj;
        foo.mNativeObj = 0;

        return do_f2(a0, a1);
    }
    private static native int do_f2(double a0, long foo);"#;
