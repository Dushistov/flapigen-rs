@@expect {"after":"\n\n    static","before":"friend class FooWrapper<false>;\n\n    ","file":"Foo.hpp","kind":"between"}
static void f1(c_fn_32c_int4232mut3232c_void_t cb) noexcept;
@@end

@@expect {"after":"\n","before":"static_assert(sizeof(uintptr_t) == sizeof(uint8_t) * 8,\n   \"our conversion usize <-> uintptr_t is wrong\");\n#endif\n            \n","file":"c_fn_32c_int4232mut3232c_void_t.h","kind":"between"}
typedef void (*c_fn_32c_int4232mut3232c_void_t)(int, void *);
@@end

@@expect {"after":"\n\n    static","before":"static void f1(c_fn_32c_int4232mut3232c_void_t cb) noexcept;\n\n    ","file":"Foo.hpp","kind":"between"}
static void f2(c_fn_4232mut3232c_void_t cb) noexcept;
@@end

@@expect {"after":"\n","before":"static_assert(sizeof(uintptr_t) == sizeof(uint8_t) * 8,\n   \"our conversion usize <-> uintptr_t is wrong\");\n#endif\n            \n","file":"c_fn_4232mut3232c_void_t.h","kind":"between"}
typedef void (*c_fn_4232mut3232c_void_t)(void *);
@@end

@@expect {"after":"\n\n};\n\n\n    template<bool OWN_DATA>\n    i","before":"static void f2(c_fn_4232mut3232c_void_t cb) noexcept;\n\n    ","file":"Foo.hpp","kind":"between"}
static void f3(c_fn_4232mut3232c_void_ret_32c_char_t cb) noexcept;
@@end

@@expect {"after":"\n        ","before":"static_assert(sizeof(uintptr_t) == sizeof(uint8_t) * 8,\n   \"our conversion usize <-> uintptr_t is wrong\");\n#endif\n            \n        ","file":"c_fn_4232mut3232c_void_ret_32c_char_t.h","kind":"between"}
typedef char (*c_fn_4232mut3232c_void_ret_32c_char_t)(void *);
@@end
