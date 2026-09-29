@@expect {"after":";\n\n    publi","before":"private static native long init(int a0);\n\n    ","file":"Foo.java","kind":"between"}
private static native void private_f()
@@end

@@expect {"after":";\n\n    prote","before":"private static native void private_f();\n\n    ","file":"Foo.java","kind":"between"}
public static native void public_f()
@@end

@@expect {"after":";\n\n    publi","before":"public static native void public_f();\n\n    ","file":"Foo.java","kind":"between"}
protected static native void protected_f()
@@end

@@expect {"after":" {\n        m","before":"private static native long init();\n\n    ","file":"Foo.java","kind":"between"}
private Foo(int a0)
@@end

@@expect {"after":" {\n        m","before":"public final class Foo {\n\n    ","file":"Foo.java","kind":"between"}
public Foo()
@@end
