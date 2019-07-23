r#"public final @NonNull Boo getBoo() {
        long ret = do_getBoo(mNativeObj);
        Boo convRet = new Boo(InternalPointerMarker.RAW_PTR, ret);

        return convRet;
    }
    private static native long do_getBoo(long self);"#;
