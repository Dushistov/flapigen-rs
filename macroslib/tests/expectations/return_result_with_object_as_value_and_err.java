r#"public static Position f1() throws Exception {
        long ret = do_f1();
        Position conv_ret = new Position(InternalPointerMarker.RAW_PTR, ret);

        return conv_ret;
    }
    private static native long do_f1() throws Exception;"#;
"public static native void f2() throws Exception;";
r#"public final Position f3() throws Exception {
        long ret = do_f3(mNativeObj);
        Position conv_ret = new Position(InternalPointerMarker.RAW_PTR, ret);

        return conv_ret;
    }
    private static native long do_f3(long self) throws Exception;"#;

r#"public static LocationService create() throws Exception {
        long ret = do_create();
        LocationService conv_ret = new LocationService(InternalPointerMarker.RAW_PTR, ret);

        return conv_ret;
    }
    private static native long do_create() throws Exception;"#;

r#"public static Foo from_string(@NonNull String a0) throws Exception {
        long ret = do_from_string(a0);
        Foo conv_ret = new Foo(InternalPointerMarker.RAW_PTR, ret);

        return conv_ret;
    }
    private static native long do_from_string(String a0) throws Exception;"#;
