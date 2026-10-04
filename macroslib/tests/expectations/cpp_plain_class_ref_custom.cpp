@@expect {"file":"Foo.hpp","kind":"item","name":"get","form":"declaration"}
FooBorrow get() const noexcept;
@@end

@@expect {"file":"Foo.hpp","kind":"item","name":"get_mut","form":"declaration"}
FooMutBorrow get_mut() noexcept;
@@end

@@expect {"file":"Foo.hpp","kind":"item","name":"Foo::get","form":"definition"}
inline FooBorrow Foo::get() const noexcept
    {

        const void * ret = Foo_get(this->self_);
        return FooBorrow{ static_cast<const FooOpaque *>(ret) };
    }
@@end

@@expect {"file":"Foo.hpp","kind":"item","name":"Foo::get_mut","form":"definition"}
inline FooMutBorrow Foo::get_mut() noexcept
    {

        void * ret = Foo_get_mut(this->self_);
        return FooMutBorrow{ static_cast<FooOpaque *>(ret) };
    }
@@end
