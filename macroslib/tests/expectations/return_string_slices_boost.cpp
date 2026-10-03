@@expect {"file":"StringSlices.hpp","kind":"item","name":"refs","form":"declaration"}
RustSlice<const boost::string_view, internal::StringSliceAccess<boost::string_view, CRustSliceStrRef, CRustSliceStrRefElem, crust_slice_str_ref_get>> refs() const noexcept;
@@end

@@expect {"file":"StringSlices.hpp","kind":"item","name":"same_refs","form":"declaration"}
bool same_refs(RustSlice<const boost::string_view, internal::StringSliceAccess<boost::string_view, CRustSliceStrRef, CRustSliceStrRefElem, crust_slice_str_ref_get>> values) const noexcept;
@@end

@@expect {"file":"StringSlices.hpp","kind":"item","name":"same_strings","form":"declaration"}
bool same_strings(RustSlice<const boost::string_view, internal::StringSliceAccess<boost::string_view, CRustSliceString, CRustSliceStringElem, crust_slice_string_get>> values) const noexcept;
@@end

@@expect {"file":"StringSlices.hpp","kind":"item","name":"same_boxed","form":"declaration"}
bool same_boxed(RustSlice<const boost::string_view, internal::StringSliceAccess<boost::string_view, CRustSliceBoxStr, CRustSliceBoxStrElem, crust_slice_box_str_get>> values) const noexcept;
@@end

@@expect {"file":"StringSlices.hpp","kind":"item","name":"strings","form":"declaration"}
RustSlice<const boost::string_view, internal::StringSliceAccess<boost::string_view, CRustSliceString, CRustSliceStringElem, crust_slice_string_get>> strings() const noexcept;
@@end

@@expect {"file":"StringSlices.hpp","kind":"item","name":"boxed","form":"declaration"}
RustSlice<const boost::string_view, internal::StringSliceAccess<boost::string_view, CRustSliceBoxStr, CRustSliceBoxStrElem, crust_slice_box_str_get>> boxed() const noexcept;
@@end
