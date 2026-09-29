@@expect {"after":" {\n        l","before":"private static native @NonNull Foo [] do_get_foo_arr(long self);\n\n    ","file":"Boo.java","kind":"between"}
public final @NonNull Foo get_foo_with_err() throws Exception
@@end

@@expect {"after":" {\n        F","before":"private static native long do_get_foo_with_err(long self) throws Exception;\n\n    ","file":"Boo.java","kind":"between"}
public final @NonNull Foo [] get_foo_arr_with_err() throws Exception
@@end

@@expect {"after":"\n        Foo","before":"private static native long init(int a0, long a1);\n\n    ","file":"Boo.java","kind":"between"}
public final @NonNull Foo [] get_foo_arr() {
@@end
