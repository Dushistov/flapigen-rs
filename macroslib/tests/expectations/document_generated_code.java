@@expect {"after":"\n    /**\n   ","before":"import android.support.annotation.NonNull;\n","file":"Foo.java","kind":"between"}
/**
 * This is class Foo
 */
public final class Foo {
@@end

@@expect {"after":"\n        mNa","before":"public final class Foo {\n    ","file":"Foo.java","kind":"between"}
/**
     * Some documentation comment
     */
    public Foo(int a0, @NonNull String a1) {
@@end

@@expect {"after":" {\n        i","before":"private static native long init(int a0, @NonNull String a1);\n    ","file":"Foo.java","kind":"between"}
/**
     * 1 Some documentation comment
     * 2 Some documentation comment
     */
    public final int f(int a0, int a1)
@@end
