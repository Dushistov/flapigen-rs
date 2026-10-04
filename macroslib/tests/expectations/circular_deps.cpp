@@expect {"after":" noexcept;\n\n","before":"            std::abort();\n        }\n    }\n\n    ","file":"A.hpp","kind":"between"}
static void a(BRef b)
@@end

@@expect {"after":" noexcept;\n\n","before":"            std::abort();\n        }\n    }\n\n    ","file":"B.hpp","kind":"between"}
static void b(ARef a)
@@end
