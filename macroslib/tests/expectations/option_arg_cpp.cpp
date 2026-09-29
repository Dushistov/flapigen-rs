@@expect {"after":" a0) const noexcept;\n\n    void f2(std::optional<Boo> a0) noexcept;\n\n    void f3(std::optional<ControlItem> a0) noexcept;\n\n    void f4(std::optional<uintptr_t> x","before":"out.is_some = 0;\n            }\n            return out;\n            }(std::move(a1)));\n        if (this->self_ == nullptr) {\n            std::abort();\n        }\n    }\n\n    ","file":"Foo.hpp","kind":"between"}
void f1(std::optional<double>
@@end

@@expect {"after":" a0) noexcept;\n\n    void","before":"void f1(std::optional<double> a0) const noexcept;\n\n    ","file":"Foo.hpp","kind":"between"}
void f2(std::optional<Boo>
@@end

@@expect {"after":"\n\n    static void f5(std","before":"void f3(std::optional<ControlItem> a0) noexcept;\n\n    ","file":"Foo.hpp","kind":"between"}
void f4(std::optional<uintptr_t> x) const noexcept;
@@end

@@expect {"after":"\n\n    static void f6(std","before":"void f4(std::optional<uintptr_t> x) const noexcept;\n\n    ","file":"Foo.hpp","kind":"between"}
static void f5(std::optional<double> x, std::optional<uintptr_t> y) noexcept;
@@end

@@expect {"after":"\n\n    void F","before":"void Foo_f5(struct CRustOptionf64 x, struct CRustOptionusize y);\n\n    ","file":"c_Foo.h","kind":"between"}
void Foo_f6(struct CRustOptionCRustStrView x);
@@end

@@expect {"after":"\n\n    static void f7(con","before":"static void f5(std::optional<double> x, std::optional<uintptr_t> y) noexcept;\n\n    ","file":"Foo.hpp","kind":"between"}
static void f6(std::optional<std::string_view> x) noexcept;
@@end

@@expect {"file":"Foo.hpp","kind":"item","name":"FooWrapper<OWN_DATA>::f6","form":"definition"}
template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::f6(std::optional<std::string_view> x) noexcept
    {

        Foo_f6([](std::optional<std::string_view> p) -> CRustOptionCRustStrView {
            CRustOptionCRustStrView out;
            if (p.has_value()) {
                out.val.data = CRustStrView{ (*p).data(), (*p).size() };
                out.is_some = 1;
            } else {
                out.is_some = 0;
            }
            return out;
            }(std::move(x)));
    }
@@end

@@expect {"after":"\n\n    void f4(std::optio","before":"void f2(std::optional<Boo> a0) noexcept;\n\n    ","file":"Foo.hpp","kind":"between"}
void f3(std::optional<ControlItem> a0) noexcept;
@@end

@@expect {"after":"\n\n    template<bool OWN_DATA>\n    inline void FooWrapper<OWN_DATA>::f4(std::optional<uintptr_t> x) const noexcept\n    {\n\n        Foo_f4(this->self_, [](std::optional<uintptr_t> p) -> CRustOptionusize {\n            CRustOptionusize out;\n            if (p.has_value()) {\n                out.val.data = (*p);\n              ","before":"Foo_f2(this->self_, [](std::optional<Boo> p) -> CRustOption4232mut3232c_void {\n            CRustOption4232mut3232c_void out;\n            if (p.has_value()) {\n                out.val.data = (*p).release();\n                out.is_some = 1;\n            } else {\n                out.is_some = 0;\n            }\n            return out;\n            }(std::move(a0)));\n    }\n\n    ","file":"Foo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::f3(std::optional<ControlItem> a0) noexcept
    {

        Foo_f3(this->self_, [](std::optional<ControlItem> p) -> CRustOptionu32 {
            CRustOptionu32 out;
            if (p.has_value()) {
                out.val.data = static_cast<uint32_t>((*p));
                out.is_some = 1;
            } else {
                out.is_some = 0;
            }
            return out;
            }(std::move(a0)));
    }
@@end

@@expect {"after":"\n\n    void F","before":"void Foo_f2(FooOpaque * const self, struct CRustOption4232mut3232c_void a0);\n\n    ","file":"c_Foo.h","kind":"between"}
void Foo_f3(FooOpaque * const self, struct CRustOptionu32 a0);
@@end

@@expect {"after":"\n\nprivate:\n   static voi","before":"static void f6(std::optional<std::string_view> x) noexcept;\n\n    ","file":"Foo.hpp","kind":"between"}
static void f7(const Boo * x) noexcept;
@@end

@@expect {"after":"\n\n} // namespace org_examples\n","before":"Foo_f6([](std::optional<std::string_view> p) -> CRustOptionCRustStrView {\n            CRustOptionCRustStrView out;\n            if (p.has_value()) {\n                out.val.data = CRustStrView{ (*p).data(), (*p).size() };\n                out.is_some = 1;\n            } else {\n                out.is_some = 0;\n            }\n            return out;\n            }(std::move(x)));\n    }\n\n    ","file":"Foo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::f7(const Boo * x) noexcept
    {

        struct CRustClassOptBoo a0 = CRustClassOptBoo { (x != nullptr) ? static_cast<BooOpaque *>(* x) : nullptr };

        Foo_f7(std::move(a0));
    }
@@end

@@expect {"after":"\n\n    void F","before":"void Foo_f6(struct CRustOptionCRustStrView x);\n\n    ","file":"c_Foo.h","kind":"between"}
void Foo_f7(struct CRustClassOptBoo x);
@@end
