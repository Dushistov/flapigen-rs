@@expect {"after":"\n\n\n    stati","before":"namespace org_examples {\n\n","file":"Foo.hpp","kind":"between"}
class Foo {
public:
    virtual ~Foo() noexcept {}

    virtual std::variant<RustString, Error> unpack(std::string_view x) const noexcept = 0;

    virtual std::optional<Error> remove() const noexcept = 0;
@@end

@@expect {"after":"\n\n#ifdef __cplusplus\n} // extern \"C\" {\n#endif\n#include <stdint.h>\n\n#ifdef __cplu","before":"\"our conversion usize <-> uintptr_t is wrong\");\n#endif\n            #include <stdint.h>\n\n#ifdef __cplusplus\nextern \"C\" {\n#endif\n","file":"rust_void_ok_result4232mut3232c_void.h","kind":"between"}
union CRustVoidOkResultUnion4232mut3232c_void {
    uint8_t ok;
    void * err;
};
@@end
