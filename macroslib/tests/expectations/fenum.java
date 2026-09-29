@@expect {"after":"\n\n    privat","before":"package org.example;\n\n\n","file":"Foo.java","kind":"between"}
public enum Foo {
    A(0),
    B(1);
@@end

@@expect {"after":"\n\n    privat","before":"package org.example;\n\n\n","file":"Boo.java","kind":"between"}
public enum Boo {
    C(0),
    D(1);
@@end
