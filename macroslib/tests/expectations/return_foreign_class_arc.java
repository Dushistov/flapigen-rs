@@expect {"after":"\n\n    public","before":"private static native long init() throws Exception;\n\n    ","file":"Moo.java","kind":"between"}
public final @NonNull Boo getBoo() {
        long ret = do_getBoo(mNativeObj);
        Boo convRet = new Boo(InternalPointerMarker.RAW_PTR, ret);

        return convRet;
    }
    private static native long do_getBoo(long self);
@@end
