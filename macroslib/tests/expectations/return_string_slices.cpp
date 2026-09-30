@@expect {"file":"rust_string_slice.h","kind":"item","name":"StringSliceAccess","form":"definition"}
struct StringSliceAccess {
    using storage_type = Element;

    static View index(SliceStorage<const Element *> slice, size_t i) noexcept
    {
        const auto str = Get(Descriptor{ slice.data, slice.len }, i);
        return str.len == 0 ? View{} : View{ str.data, str.len };
    }
};
@@end

@@expect {"file":"StringSlices.hpp","kind":"item","name":"StringSlicesWrapper<OWN_DATA>::same_refs","form":"definition"}
template<bool OWN_DATA>
    inline bool StringSlicesWrapper<OWN_DATA>::same_refs(RustSlice<const std::string_view, internal::StringSliceAccess<std::string_view, CRustSliceStrRef, CRustSliceStrRefElem, crust_slice_str_ref_get>> values) const noexcept
    {

        char ret = StringSlices_same_refs(this->self_, values.as_c<CRustSliceStrRef>());
        return (ret != 0);
    }
@@end

@@expect {"file":"StringSlices.hpp","kind":"item","name":"StringSlicesWrapper<OWN_DATA>::same_strings","form":"definition"}
template<bool OWN_DATA>
    inline bool StringSlicesWrapper<OWN_DATA>::same_strings(RustSlice<const std::string_view, internal::StringSliceAccess<std::string_view, CRustSliceString, CRustSliceStringElem, crust_slice_string_get>> values) const noexcept
    {

        char ret = StringSlices_same_strings(this->self_, values.as_c<CRustSliceString>());
        return (ret != 0);
    }
@@end

@@expect {"file":"StringSlices.hpp","kind":"item","name":"StringSlicesWrapper<OWN_DATA>::same_boxed","form":"definition"}
template<bool OWN_DATA>
    inline bool StringSlicesWrapper<OWN_DATA>::same_boxed(RustSlice<const std::string_view, internal::StringSliceAccess<std::string_view, CRustSliceBoxStr, CRustSliceBoxStrElem, crust_slice_box_str_get>> values) const noexcept
    {

        char ret = StringSlices_same_boxed(this->self_, values.as_c<CRustSliceBoxStr>());
        return (ret != 0);
    }
@@end

@@expect {"file":"rust_string_slice.h","kind":"item","name":"CRustSliceStrRef","form":"definition"}
struct CRustSliceStrRef {
    const CRustSliceStrRefElem * data;
    uintptr_t len;
};
@@end

@@expect {"file":"rust_string_slice.h","kind":"item","name":"CRustSliceString","form":"definition"}
struct CRustSliceString {
    const CRustSliceStringElem * data;
    uintptr_t len;
};
@@end

@@expect {"file":"rust_string_slice.h","kind":"item","name":"CRustSliceBoxStr","form":"definition"}
struct CRustSliceBoxStr {
    const CRustSliceBoxStrElem * data;
    uintptr_t len;
};
@@end

@@expect {"file":"rust_string_slice.h","kind":"item","name":"crust_slice_str_ref_get","form":"declaration"}
struct CRustStrView crust_slice_str_ref_get(struct CRustSliceStrRef slice, uintptr_t index);
@@end

@@expect {"file":"rust_string_slice.h","kind":"item","name":"crust_slice_string_get","form":"declaration"}
struct CRustStrView crust_slice_string_get(struct CRustSliceString slice, uintptr_t index);
@@end

@@expect {"file":"rust_string_slice.h","kind":"item","name":"crust_slice_box_str_get","form":"declaration"}
struct CRustStrView crust_slice_box_str_get(struct CRustSliceBoxStr slice, uintptr_t index);
@@end

@@expect {"file":"StringSlices.hpp","kind":"item","name":"StringSlicesWrapper<OWN_DATA>::refs","form":"definition"}
template<bool OWN_DATA>
    inline RustSlice<const std::string_view, internal::StringSliceAccess<std::string_view, CRustSliceStrRef, CRustSliceStrRefElem, crust_slice_str_ref_get>> StringSlicesWrapper<OWN_DATA>::refs() const noexcept
    {

        struct CRustSliceStrRef ret = StringSlices_refs(this->self_);
        return RustSlice<const std::string_view, internal::StringSliceAccess<std::string_view, CRustSliceStrRef, CRustSliceStrRefElem, crust_slice_str_ref_get>>{ret};
    }
@@end

@@expect {"file":"StringSlices.hpp","kind":"item","name":"StringSlicesWrapper<OWN_DATA>::strings","form":"definition"}
template<bool OWN_DATA>
    inline RustSlice<const std::string_view, internal::StringSliceAccess<std::string_view, CRustSliceString, CRustSliceStringElem, crust_slice_string_get>> StringSlicesWrapper<OWN_DATA>::strings() const noexcept
    {

        struct CRustSliceString ret = StringSlices_strings(this->self_);
        return RustSlice<const std::string_view, internal::StringSliceAccess<std::string_view, CRustSliceString, CRustSliceStringElem, crust_slice_string_get>>{ret};
    }
@@end

@@expect {"file":"StringSlices.hpp","kind":"item","name":"StringSlicesWrapper<OWN_DATA>::boxed","form":"definition"}
template<bool OWN_DATA>
    inline RustSlice<const std::string_view, internal::StringSliceAccess<std::string_view, CRustSliceBoxStr, CRustSliceBoxStrElem, crust_slice_box_str_get>> StringSlicesWrapper<OWN_DATA>::boxed() const noexcept
    {

        struct CRustSliceBoxStr ret = StringSlices_boxed(this->self_);
        return RustSlice<const std::string_view, internal::StringSliceAccess<std::string_view, CRustSliceBoxStr, CRustSliceBoxStrElem, crust_slice_box_str_get>>{ret};
    }
@@end
