@@expect {"after":"\n} // namesp","before":"namespace org_examples {\n\n","file":"Foo.hpp","kind":"between"}
enum Foo {
A = 0,
B = 1
};
@@end

@@expect {"after":"\n} // namesp","before":"namespace org_examples {\n\n","file":"Boo.hpp","kind":"between"}
enum Boo {
C = 0,
D = 1
};
@@end
