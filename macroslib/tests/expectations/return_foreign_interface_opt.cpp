@@expect {"after":" noexcept;\n\n","before":"public:\n\n    ","file":"Boo.hpp","kind":"between"}
std::variant<Foo, RustString> f()
@@end

@@expect {"after":" noexcept;\n\n","before":"std::variant<Foo, RustString> f() noexcept;\n\n    ","file":"Boo.hpp","kind":"between"}
std::optional<Foo> f2()
@@end

@@expect {"after":"\n\n    void B","before":"struct CRustResult4232mut3232c_voidCRustString Boo_f(BooOpaque * const self);\n\n    ","file":"c_Boo.h","kind":"between"}
struct CRustOption4232mut3232c_void Boo_f2(BooOpaque * const self);
@@end
