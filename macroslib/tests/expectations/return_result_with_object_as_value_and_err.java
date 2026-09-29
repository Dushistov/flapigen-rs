@@expect {"after":"\n\n    public","before":"private static native long init();\n\n    ","file":"LocationService.java","kind":"between"}
public static @NonNull Position f1() throws Exception {
        long ret = do_f1();
        Position convRet = new Position(InternalPointerMarker.RAW_PTR, ret);

        return convRet;
    }
    private static native long do_f1() throws Exception;
@@end

@@expect {"after":"\n\n    public static @Non","before":"private static native long do_f1() throws Exception;\n\n    ","file":"LocationService.java","kind":"between"}
public static native void f2() throws Exception;
@@end

@@expect {"after":"\n\n    public synchronized void delete() ","before":"public static native void f2() throws Exception;\n\n    ","file":"LocationService.java","kind":"between"}
public static @NonNull LocationService create() throws Exception {
        long ret = do_create();
        LocationService convRet = new LocationService(InternalPointerMarker.RAW_PTR, ret);

        return convRet;
    }
    private static native long do_create() throws Exception;
@@end

@@expect {"after":"\n\n    public","before":"private static native long init();\n\n    ","file":"Foo.java","kind":"between"}
public static @NonNull Foo from_string(@NonNull String a0) throws Exception {
        long ret = do_from_string(a0);
        Foo convRet = new Foo(InternalPointerMarker.RAW_PTR, ret);

        return convRet;
    }
    private static native long do_from_string(@NonNull String a0) throws Exception;
@@end
