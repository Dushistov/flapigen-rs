@@expect {"after":"\n\n    uintpt","before":"extern \"C\" {\n#endif\n\n    ","file":"c_EmptyViews.h","kind":"between"}
uintptr_t EmptyViews_take_slice(struct CRustSliceu32 value);
@@end

@@expect {"after":"\n\n    struct","before":"uintptr_t EmptyViews_take_slice(struct CRustSliceu32 value);\n\n    ","file":"c_EmptyViews.h","kind":"between"}
uintptr_t EmptyViews_take_mut_slice(struct CRustSliceMutu32 value);
@@end

@@expect {"after":"\n\n#ifdef __cplusplus\n}\n#","before":"uintptr_t EmptyViews_take_mut_slice(struct CRustSliceMutu32 value);\n\n    ","file":"c_EmptyViews.h","kind":"between"}
struct CRustString EmptyViews_take_str(struct CRustStrView value);
@@end
