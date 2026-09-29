@@expect {"after":" noexcept;\n\n","before":"            std::abort();\n        }\n    }\n\n    ","file":"LocationService.hpp","kind":"between"}
static std::variant<Position, RustString> f1()
@@end

@@expect {"after":" noexcept;\n\n    std::var","before":"static std::variant<Position, RustString> f1() noexcept;\n\n    static ","file":"LocationService.hpp","kind":"between"}
std::optional<RustString> f2()
@@end

@@expect {"after":" const noexcept;\n\n    st","before":"static std::optional<RustString> f2() noexcept;\n\n    ","file":"LocationService.hpp","kind":"between"}
std::variant<Position, PosErr> f3()
@@end

@@expect {"after":" noexcept;\n\n    static s","before":"std::variant<Position, PosErr> f3() const noexcept;\n\n    static ","file":"LocationService.hpp","kind":"between"}
std::optional<PosErr> f4()
@@end

@@expect {"after":"std::string_","before":"            std::abort();\n        }\n    }\n\n    ","file":"Foo.hpp","kind":"between"}
static std::variant<Foo, RustString> from_string(
@@end
