@@expect {"after":"\npublic:\n    using value","before":"using FooRef = FooWrapper<false>;\n\n","file":"Foo.hpp","kind":"between"}
template<bool>
struct FooWrapperCopyControl {};
template<>
struct FooWrapperCopyControl<true> {
    FooWrapperCopyControl() = default;
    FooWrapperCopyControl(const FooWrapperCopyControl&) = delete;
    FooWrapperCopyControl& operator=(const FooWrapperCopyControl&) = delete;
};
//This is class Foo
template<bool OWN_DATA>
class FooWrapper : private FooWrapperCopyControl<OWN_DATA> {
@@end

@@expect {"after":"\n    {\n\n    ","before":"static constexpr const uintptr_t &rust_elem_size = RustForeignClassFooElemSize;\n    ","file":"Foo.hpp","kind":"between"}
//Some documentation comment
    FooWrapper(int32_t a0, std::string_view a1) noexcept
@@end

@@expect {"after":"\n\nprivate:\n ","before":"            std::abort();\n        }\n    }\n    ","file":"Foo.hpp","kind":"between"}
//1 Some documentation comment
    //2 Some documentation comment
    int32_t f(int32_t a0, int32_t a1) const noexcept;
@@end
