@@expect {"after":"\n\n    RustSlice<const Bar> foreign_other","before":"const void * raw_ptr() const noexcept;\n\n    ","file":"Mix.hpp","kind":"between"}
RustSlice<const Foo> foreign_first() const noexcept;
@@end

@@expect {"after":"\n\n    BarRef","before":"            std::abort();\n        }\n    }\n\n    ","file":"Mix.hpp","kind":"between"}
FooRef foo_ref() const noexcept;
@@end

@@expect {"after":"\n\n    const ","before":"FooRef foo_ref() const noexcept;\n\n    ","file":"Mix.hpp","kind":"between"}
BarRef bar_ref() const noexcept;
@@end

@@expect {"after":"\n\n    RustSlice<const Foo> foreign_first","before":"  BarRef bar_ref() const noexcept;\n\n    ","file":"Mix.hpp","kind":"between"}
const void * raw_ptr() const noexcept;
@@end

@@expect {"after":"\n\n    RustSlice<const uint64_t> native_o","before":"RustSlice<const Bar> foreign_other() const noexcept;\n\n    ","file":"Mix.hpp","kind":"between"}
RustSlice<const uint32_t> native_second() const noexcept;
@@end

@@expect {"after":"\n\n    void mut_foreign_other(RustSlice<B","before":"RustSlice<const uint64_t> native_other() const noexcept;\n\n    ","file":"Mix.hpp","kind":"between"}
void mut_foreign(RustSlice<Foo> values) const noexcept;
@@end

@@expect {"after":"\n\n    void mut_native_other(RustSlice<ui","before":"void mut_foreign_other(RustSlice<Bar> values) const noexcept;\n\n    ","file":"Mix.hpp","kind":"between"}
void mut_native(RustSlice<uint32_t> values) const noexcept;
@@end

@@expect {"after":"\n\n    struct CRustSliceForeignBar Mix_fo","before":"const void * Mix_raw_ptr(const MixOpaque * const self);\n\n    ","file":"c_Mix.h","kind":"between"}
struct CRustSliceForeignFoo Mix_foreign_first(const MixOpaque * const self);
@@end

@@expect {"after":"\n\n    const ","before":"MixOpaque *Mix_new();\n\n    ","file":"c_Mix.h","kind":"between"}
const FooOpaque * Mix_foo_ref(const MixOpaque * const self);
@@end

@@expect {"after":"\n\n    const ","before":"const FooOpaque * Mix_foo_ref(const MixOpaque * const self);\n\n    ","file":"c_Mix.h","kind":"between"}
const BarOpaque * Mix_bar_ref(const MixOpaque * const self);
@@end

@@expect {"after":"\n\n    struct CRustSliceForeignFoo Mix_foreign_first(const MixOpaque * const self","before":"const FooOpaque * Mix_foo_ref(const MixOpaque * const self);\n\n    const BarOpaque * Mix_bar_ref(const MixOpaque * const self);\n\n    ","file":"c_Mix.h","kind":"between"}
const void * Mix_raw_ptr(const MixOpaque * const self);
@@end

@@expect {"after":"\n\n    struct CRustSliceu64 Mix_native_ot","before":"struct CRustSliceForeignBar Mix_foreign_other(const MixOpaque * const self);\n\n    ","file":"c_Mix.h","kind":"between"}
struct CRustSliceu32 Mix_native_second(const MixOpaque * const self);
@@end

@@expect {"after":"\n\n    void Mix_mut_foreign_other(const MixOpaque * const self, struct CRustSlice","before":"struct CRustSliceu32 Mix_native_second(const MixOpaque * const self);\n\n    struct CRustSliceu64 Mix_native_other(const MixOpaque * const self);\n\n    ","file":"c_Mix.h","kind":"between"}
void Mix_mut_foreign(const MixOpaque * const self, struct CRustSliceMutForeignFoo values);
@@end

@@expect {"after":"\n\n    void Mix_mut_nativ","before":"void Mix_mut_foreign_other(const MixOpaque * const self, struct CRustSliceMutForeignBar values);\n\n    ","file":"c_Mix.h","kind":"between"}
void Mix_mut_native(const MixOpaque * const self, struct CRustSliceMutu32 values);
@@end

@@expect {"after":"\n\n    RustSlice<const uint32_t> native_s","before":"RustSlice<const Foo> foreign_first() const noexcept;\n\n    ","file":"Mix.hpp","kind":"between"}
RustSlice<const Bar> foreign_other() const noexcept;
@@end

@@expect {"after":"\n\n    void mut_foreign(RustSlice<Foo> va","before":"RustSlice<const uint32_t> native_second() const noexcept;\n\n    ","file":"Mix.hpp","kind":"between"}
RustSlice<const uint64_t> native_other() const noexcept;
@@end

@@expect {"after":"\n\n    void mut_native(Ru","before":"void mut_foreign(RustSlice<Foo> values) const noexcept;\n\n    ","file":"Mix.hpp","kind":"between"}
void mut_foreign_other(RustSlice<Bar> values) const noexcept;
@@end

@@expect {"after":"\n\nprivate:\n   static void free_mem(SelfT","before":"void mut_native(RustSlice<uint32_t> values) const noexcept;\n\n    ","file":"Mix.hpp","kind":"between"}
void mut_native_other(RustSlice<uint64_t> values) const noexcept;
@@end

@@expect {"after":"\n\n    struct CRustSliceu32 Mix_native_se","before":"struct CRustSliceForeignFoo Mix_foreign_first(const MixOpaque * const self);\n\n    ","file":"c_Mix.h","kind":"between"}
struct CRustSliceForeignBar Mix_foreign_other(const MixOpaque * const self);
@@end

@@expect {"after":"\n\n    void Mix_mut_foreign(const MixOpaq","before":"struct CRustSliceu32 Mix_native_second(const MixOpaque * const self);\n\n    ","file":"c_Mix.h","kind":"between"}
struct CRustSliceu64 Mix_native_other(const MixOpaque * const self);
@@end

@@expect {"after":"\n\n    void Mix_mut_nativ","before":"void Mix_mut_foreign(const MixOpaque * const self, struct CRustSliceMutForeignFoo values);\n\n    ","file":"c_Mix.h","kind":"between"}
void Mix_mut_foreign_other(const MixOpaque * const self, struct CRustSliceMutForeignBar values);
@@end

@@expect {"after":"\n\n    void Mix_delete(co","before":"void Mix_mut_native(const MixOpaque * const self, struct CRustSliceMutu32 values);\n\n    ","file":"c_Mix.h","kind":"between"}
void Mix_mut_native_other(const MixOpaque * const self, struct CRustSliceMutu64 values);
@@end

@@expect {"after":"\n\n#ifdef __c","before":"extern \"C\" {\n#endif\n","file":"CRustSliceForeignFoo.h","kind":"between"}
struct CRustSliceForeignFoo {
    const struct FooOpaque * data;
    uintptr_t len;
};
@@end

@@expect {"after":"\n\n#ifdef __c","before":"extern \"C\" {\n#endif\n","file":"CRustSliceForeignBar.h","kind":"between"}
struct CRustSliceForeignBar {
    const struct BarOpaque * data;
    uintptr_t len;
};
@@end

@@expect {"after":"\n\n#ifdef __c","before":"extern \"C\" {\n#endif\n","file":"CRustSliceMutForeignFoo.h","kind":"between"}
struct CRustSliceMutForeignFoo {
    struct FooOpaque * data;
    uintptr_t len;
};
@@end

@@expect {"after":"\n\n#ifdef __c","before":"extern \"C\" {\n#endif\n","file":"CRustSliceu32.h","kind":"between"}
struct CRustSliceu32 {
    const uint32_t * data;
    uintptr_t len;
};
@@end

@@expect {"after":"\n\n#ifdef __c","before":"extern \"C\" {\n#endif\n","file":"CRustSliceMutu32.h","kind":"between"}
struct CRustSliceMutu32 {
    uint32_t * data;
    uintptr_t len;
};
@@end
