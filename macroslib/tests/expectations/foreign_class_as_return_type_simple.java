@@expect {"after":"\n\n    public","before":"private static native long init(int a0, long a1);\n\n    ","file":"Boo.java","kind":"between"}
public static @NonNull Boo factory_method() {
        long ret = do_factory_method();
        Boo convRet = new Boo(InternalPointerMarker.RAW_PTR, ret);

        return convRet;
    }
    private static native long do_factory_method();
@@end

@@expect {"after":"\n        lon","before":"private static native long do_factory_method();\n\n    ","file":"Boo.java","kind":"between"}
public final @NonNull Foo get_one_foo() {
@@end

@@expect {"after":" {\n        i","before":"private static native long init(int a0);\n\n    ","file":"Foo.java","kind":"between"}
public final int f(int a0, int a1)
@@end
