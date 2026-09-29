@@expect {"after":"\n        mNa","before":"public final class Boo {\n\n    ","file":"Boo.java","kind":"between"}
public Boo() {
@@end

@@expect {"after":"\n        flo","before":"private static native long init();\n\n    ","file":"Boo.java","kind":"between"}
public final float test(boolean a0) {
@@end

@@expect {"after":"\n        do_","before":"private static native float do_test(long self, boolean a0);\n\n    ","file":"Boo.java","kind":"between"}
public final void set_a(int a0) {
@@end

@@expect {"after":"\n        mNa","before":"public final class Moo {\n\n    ","file":"Moo.java","kind":"between"}
public Moo() throws Exception {
@@end

@@expect {"after":"\n        lon","before":"private static native long init() throws Exception;\n\n    ","file":"Moo.java","kind":"between"}
public final @NonNull Boo getBoo() {
@@end
