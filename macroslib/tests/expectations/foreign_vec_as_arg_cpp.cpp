@@expect {"after":"\n\n    void s","before":"            std::abort();\n        }\n    }\n\n    ","file":"FooImpl.hpp","kind":"between"}
RustSlice<const Boo> alternateBoarding() const noexcept;
@@end

@@expect {"after":"\n\n    void F","before":"FooImplOpaque *FooImpl_create();\n\n    ","file":"c_FooImpl.h","kind":"between"}
struct CRustSliceForeignBoo FooImpl_alternateBoarding(const FooImplOpaque * const self);
@@end

@@expect {"after":"\n\n    templa","before":"constexpr const uintptr_t &FooImplWrapper<OWN_DATA>::rust_elem_size;\n\n\n    ","file":"FooImpl.hpp","kind":"between"}
template<bool OWN_DATA>
    inline RustSlice<const Boo> FooImplWrapper<OWN_DATA>::alternateBoarding() const noexcept
    {

        struct CRustSliceForeignBoo ret = FooImpl_alternateBoarding(this->self_);
        return RustSlice<const Boo>{ret};
    }
@@end

@@expect {"after":";\n\nprivate:\n","before":"RustSlice<const Boo> alternateBoarding() const noexcept;\n\n    ","file":"FooImpl.hpp","kind":"between"}
void setAlternateBoarding(RustForeignVecBoo p) noexcept
@@end

@@expect {"after":"\n\n    void F","before":"struct CRustSliceForeignBoo FooImpl_alternateBoarding(const FooImplOpaque * const self);\n\n    ","file":"c_FooImpl.h","kind":"between"}
void FooImpl_setAlternateBoarding(FooImplOpaque * const self, struct CRustForeignVec p);
@@end

@@expect {"after":"\n\n} // namespace org_exa","before":"return RustSlice<const Boo>{ret};\n    }\n\n    ","file":"FooImpl.hpp","kind":"between"}
template<bool OWN_DATA>
    inline void FooImplWrapper<OWN_DATA>::setAlternateBoarding(RustForeignVecBoo p) noexcept
    {

        FooImpl_setAlternateBoarding(this->self_, p.release());
    }
@@end
