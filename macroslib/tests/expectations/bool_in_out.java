@@expect {"after":" {\n        boolean ret =","before":"private static native long init(boolean a0);\n\n    public final ","file":"Foo.java","kind":"between"}
boolean f1(boolean a0)
@@end

@@expect {"after":";\n\n    public synchroniz","before":"private static native boolean do_f1(long self, boolean a0);\n\n    ","file":"Foo.java","kind":"between"}
public static native boolean f2(boolean a0)
@@end

@@expect {"after":" {\n        m","before":"public final class Foo {\n\n    public ","file":"Foo.java","kind":"between"}
Foo(boolean a0)
@@end

@@expect {"after":"\n","before":"package org.example;\n\n\n","file":"SomeObserver.java","greedy_match":true,"kind":"between"}
public interface SomeObserver {


    void onStateChanged1(int a0, boolean a1);

}
@@end
