@@expect {"file":"Foo.hpp","kind":"item","name":"FooWrapper<OWN_DATA>::consume","form":"definition"}
template<bool OWN_DATA>
    inline void FooWrapper<OWN_DATA>::consume(RustSlice<const Boo> values) noexcept
    {

        Foo_consume(values.as_c<CRustSliceForeignBoo>());
    }
@@end
