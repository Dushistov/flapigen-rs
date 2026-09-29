@@expect {"after":" noexcept;\n\n","before":"public:\n\n    ","file":"Boo.hpp","kind":"between"}
std::variant<Moo, Foo> f() const
@@end

@@expect {"after":"\n\nprivate:\n ","before":"std::variant<Moo, Foo> f() const noexcept;\n\n    ","file":"Boo.hpp","kind":"between"}
Foo f2(Foo a0) const noexcept;
@@end

@@expect {"after":"\n\n    templa","before":"constexpr const uintptr_t &BooWrapper<OWN_DATA>::rust_elem_size;\n\n\n    ","file":"Boo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline std::variant<Moo, Foo> BooWrapper<OWN_DATA>::f() const noexcept
    {

        struct CRustResult4232mut3232c_voidu32 ret = Boo_f(this->self_);
        return ret.is_ok != 0 ?
              std::variant<Moo, Foo> { Moo(static_cast<MooOpaque *>(ret.data.ok)) } :
              std::variant<Moo, Foo> { static_cast<Foo>(ret.data.err) };
    }
@@end

@@expect {"after":"\n\n} // names","before":"std::variant<Moo, Foo> { static_cast<Foo>(ret.data.err) };\n    }\n\n    ","file":"Boo.hpp","kind":"between"}
template<bool OWN_DATA>
    inline Foo BooWrapper<OWN_DATA>::f2(Foo a0) const noexcept
    {

        uint32_t ret = Boo_f2(this->self_, static_cast<uint32_t>(a0));
        return static_cast<Foo>(ret);
    }
@@end
