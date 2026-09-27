"RustSlice<const Foo> foreign_first() const noexcept;";
"FooRef foo_ref() const noexcept;";
"BarRef bar_ref() const noexcept;";
"const void * raw_ptr() const noexcept;";
"RustSlice<const uint32_t> native_second() const noexcept;";
"void mut_foreign(RustSlice<Foo> values) const noexcept;";
"void mut_native(RustSlice<uint32_t> values) const noexcept;";
"struct CRustSliceForeignFoo Mix_foreign_first(const MixOpaque * const self);";
"const FooOpaque * Mix_foo_ref(const MixOpaque * const self);";
"const BarOpaque * Mix_bar_ref(const MixOpaque * const self);";
"const void * Mix_raw_ptr(const MixOpaque * const self);";
"struct CRustSliceu32 Mix_native_second(const MixOpaque * const self);";
"void Mix_mut_foreign(const MixOpaque * const self, struct CRustSliceMutForeignFoo values);";
"void Mix_mut_native(const MixOpaque * const self, struct CRustSliceMutu32 values);";
"RustSlice<const Bar> foreign_other() const noexcept;";
"RustSlice<const uint64_t> native_other() const noexcept;";
"void mut_foreign_other(RustSlice<Bar> values) const noexcept;";
"void mut_native_other(RustSlice<uint64_t> values) const noexcept;";
"struct CRustSliceForeignBar Mix_foreign_other(const MixOpaque * const self);";
"struct CRustSliceu64 Mix_native_other(const MixOpaque * const self);";
"void Mix_mut_foreign_other(const MixOpaque * const self, struct CRustSliceMutForeignBar values);";
"void Mix_mut_native_other(const MixOpaque * const self, struct CRustSliceMutu64 values);";
r#"struct CRustSliceForeignFoo {
    const struct FooOpaque * data;
    uintptr_t len;
};"#;
r#"struct CRustSliceForeignBar {
    const struct BarOpaque * data;
    uintptr_t len;
};"#;
r#"struct CRustSliceMutForeignFoo {
    struct FooOpaque * data;
    uintptr_t len;
};"#;
r#"struct CRustSliceu32 {
    const uint32_t * data;
    uintptr_t len;
};"#;
r#"struct CRustSliceMutu32 {
    uint32_t * data;
    uintptr_t len;
};"#;
