"RustForeignSliceConst<BooRef> alternateBoarding() const noexcept;";
"struct CRustObjectSlice FooImpl_alternateBoarding(const FooImplOpaque * const self);";
r#"template<bool OWN_DATA>
    inline RustForeignSliceConst<BooRef> FooImplWrapper<OWN_DATA>::alternateBoarding() const noexcept
    {

        struct CRustObjectSlice ret = FooImpl_alternateBoarding(this->self_);
        return RustForeignSliceConst<BooRef>{ret};
    }"#;

"void setAlternateBoarding(RustForeignVecBoo p) noexcept";
"void FooImpl_setAlternateBoarding(FooImplOpaque * const self, struct CRustForeignVec p);";
r#"template<bool OWN_DATA>
    inline void FooImplWrapper<OWN_DATA>::setAlternateBoarding(RustForeignVecBoo p) noexcept
    {

        FooImpl_setAlternateBoarding(this->self_, p.release());
    }"#;
