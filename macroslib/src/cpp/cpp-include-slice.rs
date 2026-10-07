#[allow(dead_code)]
#[repr(C)]
pub struct CRustSliceStrRefElem { _unused: u8 }

#[allow(dead_code)]
#[repr(C)]
#[derive(Clone, Copy)]
pub struct CRustSliceStrRef {
    data: *const CRustSliceStrRefElem,
    len: usize,
}

#[allow(dead_code)]
#[repr(C)]
pub struct CRustSliceStringElem { _unused: u8 }

#[allow(dead_code)]
#[repr(C)]
#[derive(Clone, Copy)]
pub struct CRustSliceString {
    data: *const CRustSliceStringElem,
    len: usize,
}

#[allow(dead_code)]
#[repr(C)]
pub struct CRustSliceBoxStrElem { _unused: u8 }

#[allow(dead_code)]
#[repr(C)]
#[derive(Clone, Copy)]
pub struct CRustSliceBoxStr {
    data: *const CRustSliceBoxStrElem,
    len: usize,
}

foreign_typemap!(
    foreign_code!(module = "rust_string_slice.h";
                    r##"
typedef struct CRustSliceStrRefElem CRustSliceStrRefElem;
typedef struct CRustSliceStringElem CRustSliceStringElem;
typedef struct CRustSliceBoxStrElem CRustSliceBoxStrElem;

#ifdef __cplusplus
#include "rust_slice_tmpl.hpp"
#include "rust_str.h"

namespace $RUST_SWIG_USER_NAMESPACE {
namespace internal {
template <typename View, typename Descriptor, typename Element,
          CRustStrView (*Get)(Descriptor, uintptr_t)>
struct StringSliceAccess {
    using storage_type = Element;

    static View index(SliceStorage<const Element *> slice, size_t i) noexcept
    {
        const auto str = Get(Descriptor{ slice.data, slice.len }, i);
        return str.len == 0 ? View{} : View{ str.data, str.len };
    }
};
} // namespace internal
} // namespace $RUST_SWIG_USER_NAMESPACE
#endif
"##);
);

foreign_typemap!(
    define_c_type!(module = "rust_string_slice.h";
        #[repr(C)]
        pub struct CRustSliceStrRefElem { _unused: u8 }
        #[repr(C)]
        #[derive(Clone, Copy)]
        pub struct CRustSliceStrRef {
            data: *const CRustSliceStrRefElem,
            len: usize,
        }
        #[unsafe(no_mangle)]
        pub extern "C" fn crust_slice_str_ref_get(slice: CRustSliceStrRef, index: usize) -> CRustStrView {
            let values = unsafe { (CRustSlice { data: slice.data.cast(), len: slice.len }).as_slice::<&str>() };
            CRustStrView::from_str(values[index])
        }
    );
    ($p:r_type) &[&str] => CRustSliceStrRef {
        $out = CRustSliceStrRef { data: $p.as_ptr().cast(), len: $p.len() };
    };
    ($p:r_type) &[&str] <= CRustSliceStrRef {
        $out = unsafe { (CRustSlice { data: $p.data.cast(), len: $p.len }).as_slice::<&str>() };
    };
    ($p:f_type, option = "CppStrView::Std17", req_modules = ["\"rust_string_slice.h\""]) => "RustSlice<const std::string_view, internal::StringSliceAccess<std::string_view, CRustSliceStrRef, CRustSliceStrRefElem, crust_slice_str_ref_get>>"
        "RustSlice<const std::string_view, internal::StringSliceAccess<std::string_view, CRustSliceStrRef, CRustSliceStrRefElem, crust_slice_str_ref_get>>{$p}";
    ($p:f_type, option = "CppStrView::Std17", req_modules = ["\"rust_string_slice.h\""]) <= "RustSlice<const std::string_view, internal::StringSliceAccess<std::string_view, CRustSliceStrRef, CRustSliceStrRefElem, crust_slice_str_ref_get>>"
        "$p.as_c<CRustSliceStrRef>()";
    ($p:f_type, option = "CppStrView::Boost", req_modules = ["\"rust_string_slice.h\""]) => "RustSlice<const boost::string_view, internal::StringSliceAccess<boost::string_view, CRustSliceStrRef, CRustSliceStrRefElem, crust_slice_str_ref_get>>"
        "RustSlice<const boost::string_view, internal::StringSliceAccess<boost::string_view, CRustSliceStrRef, CRustSliceStrRefElem, crust_slice_str_ref_get>>{$p}";
    ($p:f_type, option = "CppStrView::Boost", req_modules = ["\"rust_string_slice.h\""]) <= "RustSlice<const boost::string_view, internal::StringSliceAccess<boost::string_view, CRustSliceStrRef, CRustSliceStrRefElem, crust_slice_str_ref_get>>"
        "$p.as_c<CRustSliceStrRef>()";
);

foreign_typemap!(
    define_c_type!(module = "rust_string_slice.h";
        #[repr(C)]
        pub struct CRustSliceStringElem { _unused: u8 }
        #[repr(C)]
        #[derive(Clone, Copy)]
        pub struct CRustSliceString {
            data: *const CRustSliceStringElem,
            len: usize,
        }
        #[unsafe(no_mangle)]
        pub extern "C" fn crust_slice_string_get(slice: CRustSliceString, index: usize) -> CRustStrView {
            let values = unsafe { (CRustSlice { data: slice.data.cast(), len: slice.len }).as_slice::<String>() };
            CRustStrView::from_str(values[index].as_str())
        }
    );
    ($p:r_type) &[String] => CRustSliceString {
        $out = CRustSliceString { data: $p.as_ptr().cast(), len: $p.len() };
    };
    ($p:r_type) &[String] <= CRustSliceString {
        $out = unsafe { (CRustSlice { data: $p.data.cast(), len: $p.len }).as_slice::<String>() };
    };
    ($p:f_type, option = "CppStrView::Std17", req_modules = ["\"rust_string_slice.h\""]) => "RustSlice<const std::string_view, internal::StringSliceAccess<std::string_view, CRustSliceString, CRustSliceStringElem, crust_slice_string_get>>"
        "RustSlice<const std::string_view, internal::StringSliceAccess<std::string_view, CRustSliceString, CRustSliceStringElem, crust_slice_string_get>>{$p}";
    ($p:f_type, option = "CppStrView::Std17", req_modules = ["\"rust_string_slice.h\""]) <= "RustSlice<const std::string_view, internal::StringSliceAccess<std::string_view, CRustSliceString, CRustSliceStringElem, crust_slice_string_get>>"
        "$p.as_c<CRustSliceString>()";
    ($p:f_type, option = "CppStrView::Boost", req_modules = ["\"rust_string_slice.h\""]) => "RustSlice<const boost::string_view, internal::StringSliceAccess<boost::string_view, CRustSliceString, CRustSliceStringElem, crust_slice_string_get>>"
        "RustSlice<const boost::string_view, internal::StringSliceAccess<boost::string_view, CRustSliceString, CRustSliceStringElem, crust_slice_string_get>>{$p}";
    ($p:f_type, option = "CppStrView::Boost", req_modules = ["\"rust_string_slice.h\""]) <= "RustSlice<const boost::string_view, internal::StringSliceAccess<boost::string_view, CRustSliceString, CRustSliceStringElem, crust_slice_string_get>>"
        "$p.as_c<CRustSliceString>()";
);

foreign_typemap!(
    define_c_type!(module = "rust_string_slice.h";
        #[repr(C)]
        pub struct CRustSliceBoxStrElem { _unused: u8 }
        #[repr(C)]
        #[derive(Clone, Copy)]
        pub struct CRustSliceBoxStr {
            data: *const CRustSliceBoxStrElem,
            len: usize,
        }
        #[unsafe(no_mangle)]
        pub extern "C" fn crust_slice_box_str_get(slice: CRustSliceBoxStr, index: usize) -> CRustStrView {
            let values = unsafe { (CRustSlice { data: slice.data.cast(), len: slice.len }).as_slice::<Box<str>>() };
            CRustStrView::from_str(values[index].as_ref())
        }
    );
    ($p:r_type) &[Box<str>] => CRustSliceBoxStr {
        $out = CRustSliceBoxStr { data: $p.as_ptr().cast(), len: $p.len() };
    };
    ($p:r_type) &[Box<str>] <= CRustSliceBoxStr {
        $out = unsafe { (CRustSlice { data: $p.data.cast(), len: $p.len }).as_slice::<Box<str>>() };
    };
    ($p:f_type, option = "CppStrView::Std17", req_modules = ["\"rust_string_slice.h\""]) => "RustSlice<const std::string_view, internal::StringSliceAccess<std::string_view, CRustSliceBoxStr, CRustSliceBoxStrElem, crust_slice_box_str_get>>"
        "RustSlice<const std::string_view, internal::StringSliceAccess<std::string_view, CRustSliceBoxStr, CRustSliceBoxStrElem, crust_slice_box_str_get>>{$p}";
    ($p:f_type, option = "CppStrView::Std17", req_modules = ["\"rust_string_slice.h\""]) <= "RustSlice<const std::string_view, internal::StringSliceAccess<std::string_view, CRustSliceBoxStr, CRustSliceBoxStrElem, crust_slice_box_str_get>>"
        "$p.as_c<CRustSliceBoxStr>()";
    ($p:f_type, option = "CppStrView::Boost", req_modules = ["\"rust_string_slice.h\""]) => "RustSlice<const boost::string_view, internal::StringSliceAccess<boost::string_view, CRustSliceBoxStr, CRustSliceBoxStrElem, crust_slice_box_str_get>>"
        "RustSlice<const boost::string_view, internal::StringSliceAccess<boost::string_view, CRustSliceBoxStr, CRustSliceBoxStrElem, crust_slice_box_str_get>>{$p}";
    ($p:f_type, option = "CppStrView::Boost", req_modules = ["\"rust_string_slice.h\""]) <= "RustSlice<const boost::string_view, internal::StringSliceAccess<boost::string_view, CRustSliceBoxStr, CRustSliceBoxStrElem, crust_slice_box_str_get>>"
        "$p.as_c<CRustSliceBoxStr>()";
);

#[allow(dead_code)]
#[repr(C)]
#[derive(Clone, Copy)]
pub struct CRustSlice {
    data: *const ::std::os::raw::c_void,
    len: usize,
}

#[allow(dead_code)]
impl CRustSlice {
    #[inline]
    pub const fn from_slice<T>(slice: &[T]) -> Self {
        Self {
            data: slice.as_ptr().cast(),
            len: slice.len(),
        }
    }

    #[inline]
    pub const unsafe fn as_slice<'a, T>(self) -> &'a [T] {
        if self.len == 0 {
            &[]
        } else {
            assert!(!self.data.is_null());
            unsafe { ::std::slice::from_raw_parts(self.data.cast(), self.len) }
        }
    }
}

#[allow(dead_code)]
#[repr(C)]
#[derive(Clone, Copy)]
pub struct CRustSliceMut {
    data: *mut ::std::os::raw::c_void,
    len: usize,
}

#[allow(dead_code)]
impl CRustSliceMut {
    #[inline]
    pub const fn from_slice<T>(slice: &mut [T]) -> Self {
        Self {
            data: slice.as_mut_ptr().cast(),
            len: slice.len(),
        }
    }

    #[inline]
    pub const unsafe fn as_slice_mut<'a, T>(self) -> &'a mut [T] {
        if self.len == 0 {
            &mut []
        } else {
            assert!(!self.data.is_null());
            unsafe { ::std::slice::from_raw_parts_mut(self.data.cast(), self.len) }
        }
    }
}

foreign_typemap!(
    foreign_code!(module = "rust_slice.h";
                    r##"
#ifdef __cplusplus
#include "rust_slice_tmpl.hpp"
#endif
"##);
);

foreign_typemap!(
    generic_alias!(CSlice = swig_concat_idents!(CRustSliceForeign, swig_f_type!(T)));
    define_c_type!(
        module = "CSlice!().h";
        #[repr(C)]
        #[derive(Clone, Copy)]
        pub struct CSlice!() {
            data: *const swig_subst_type!(T),
            len: usize,
        }
    );
    foreign_code!(module = "CSlice!().h";
                    r##"
#ifdef __cplusplus
#include "rust_slice_tmpl.hpp"
#include "swig_f_type!(T)_fwd.hpp"
#endif
"##);
    ($p:r_type) <T: SwigForeignClassDirectAccess> &[T] => CSlice!() {
        $out = CSlice!() {
            data: $p.as_ptr(),
            len: $p.len(),
        };
    };
    ($p:r_type) <T: SwigForeignClassDirectAccess> &[T] <= CSlice!() {
        $out = unsafe { (CRustSlice { data: $p.data.cast(), len: $p.len }).as_slice::<swig_subst_type!(T)>() };
    };
    ($p:f_type, req_modules = ["\"CSlice!().h\""]) => "RustSlice<const swig_f_type!(T, output)>"
        "RustSlice<const swig_f_type!(T, output)>{$p}";
    ($p:f_type, req_modules = ["\"CSlice!().h\""]) <= "RustSlice<const swig_f_type!(T, output)>"
        "$p.as_c<CSlice!()>()";
);

// Arc<T> and Rc<T> elements are pointers to T, not inline T objects. The
// accessor dereferences the smart pointer in Rust before constructing a C++
// borrowed wrapper. Each class gets its own C descriptor to keep unrelated
// slice conversions separate in the typemap graph.
foreign_typemap!(
    generic_alias!(CSlice = swig_concat_idents!(CRustSliceForeignIndirect, swig_f_type!(T)));
    generic_alias!(CSliceElem = swig_concat_idents!(CRustSliceForeignIndirect, swig_f_type!(T), Elem));
    generic_alias!(CSliceAccess = swig_concat_idents!(swig_f_type!(T), Access));
    generic_alias!(CSliceGet = swig_concat_idents!(CRustSliceForeignIndirect, swig_f_type!(T), _get));
    define_c_type!(
        module = "CSlice!().h";
        #[repr(C)]
        pub struct CSliceElem!() { _unused: u8 }

        #[repr(C)]
        #[derive(Clone, Copy)]
        pub struct CSlice!() {
            data: *const CSliceElem!(),
            len: usize,
        }

        #[allow(non_snake_case)]
        #[unsafe(no_mangle)]
        pub extern "C" fn CSliceGet!()(slice: CSlice!(), idx: usize) -> *const ::std::os::raw::c_void {
            let slice: &[swig_subst_type!(T)] = unsafe {
                (CRustSlice { data: slice.data.cast(), len: slice.len }).as_slice()
            };
            let elem_ref = &*slice[idx];
            elem_ref as *const _ as *const ::std::os::raw::c_void
        }
    );
    foreign_code!(module = "CSlice!().h";
                    r##"
#ifdef __cplusplus
#include "rust_slice_tmpl.hpp"
#include "swig_f_type!(T)_fwd.hpp"

namespace $RUST_SWIG_USER_NAMESPACE {
template<bool> class swig_f_type!(T)Wrapper;
struct CSliceAccess!() {
    using storage_type = CSliceElem!();
    template<typename Ref = swig_f_type!(T)Wrapper<false>>
    static Ref index(internal::SliceStorage<const storage_type *> slice, size_t idx) noexcept {
        auto p = static_cast<const typename Ref::CForeignType *>(
            CSliceGet!()(CSlice!(){slice.data, slice.len}, idx));
        return Ref{p};
    }
};
}
#endif
"##);
    ($p:r_type) <T: SwigForeignClassIndirectAccess> &[T] => CSlice!() {
        $out = CSlice!() { data: $p.as_ptr().cast(), len: $p.len() };
    };
    ($p:r_type) <T: SwigForeignClassIndirectAccess> &[T] <= CSlice!() {
        $out = unsafe { (CRustSlice { data: $p.data.cast(), len: $p.len }).as_slice::<swig_subst_type!(T)>() };
    };
    ($p:f_type, req_modules = ["\"CSlice!().h\""]) => "RustSlice<const swig_f_type!(T, output), CSliceAccess!()>"
        "RustSlice<const swig_f_type!(T, output), CSliceAccess!()>{$p}";
    ($p:f_type, req_modules = ["\"CSlice!().h\""]) <= "RustSlice<const swig_f_type!(T, output), CSliceAccess!()>"
        "$p.as_c<CSlice!()>()";
);

foreign_typemap!(
    generic_alias!(CSliceMut = swig_concat_idents!(CRustSliceMutForeign, swig_f_type!(T)));
    define_c_type!(
        module = "CSliceMut!().h";
        #[repr(C)]
        #[derive(Clone, Copy)]
        pub struct CSliceMut!() {
            data: *mut swig_subst_type!(T),
            len: usize,
        }
    );
    foreign_code!(module = "CSliceMut!().h";
                    r##"
#ifdef __cplusplus
#include "rust_slice_tmpl.hpp"
#include "swig_f_type!(T)_fwd.hpp"
#endif
"##);
    ($p:r_type) <T: SwigForeignClassDirectAccess> &mut [T] => CSliceMut!() {
        $out = CSliceMut!() {
            data: $p.as_mut_ptr(),
            len: $p.len(),
        };
    };
    ($p:r_type) <T: SwigForeignClassDirectAccess> &mut [T] <= CSliceMut!() {
        $out = unsafe { (CRustSliceMut { data: $p.data.cast(), len: $p.len }).as_slice_mut::<swig_subst_type!(T)>() };
    };
    ($p:f_type, req_modules = ["\"CSliceMut!().h\""]) => "RustSlice<swig_f_type!(T, output)>"
        "RustSlice<swig_f_type!(T, output)>{$p}";
    ($p:f_type, req_modules = ["\"CSliceMut!().h\""]) <= "RustSlice<swig_f_type!(T, output)>"
        "$p.as_c<CSliceMut!()>()";
);

foreign_typemap!(
    generic_alias!(CSlice = swig_concat_idents!(CRustSlice, swig_i_type!(T)));
    define_c_type!(
        module = "CSlice!().h";
        #[repr(C)]
        #[derive(Clone, Copy)]
        pub struct CSlice!() {
            data: *const swig_subst_type!(T),
            len: usize,
        }
    );
    foreign_code!(module = "CSlice!().h";
                    r##"
#ifdef __cplusplus
#include "rust_slice_tmpl.hpp"
#endif
"##);
    ($p:r_type) <T: SwigTypeIsReprC> &[T] => CSlice!() {
        $out = CSlice!() {
            data: $p.as_ptr(),
            len: $p.len(),
        };
    };
    ($p:r_type) <T: SwigTypeIsReprC> &[T] <= CSlice!() {
        $out = unsafe { (CRustSlice { data: $p.data.cast(), len: $p.len }).as_slice::<swig_subst_type!(T)>() };
    };
    ($p:f_type, req_modules = ["\"CSlice!().h\""]) => "RustSlice<const swig_f_type!(T)>"
        "RustSlice<const swig_f_type!(T)>{$p}";
    ($p:f_type, req_modules = ["\"CSlice!().h\""]) <= "RustSlice<const swig_f_type!(T)>"
        "$p.as_c<CSlice!()>()";
);

foreign_typemap!(
    generic_alias!(CSliceMut = swig_concat_idents!(CRustSliceMut, swig_i_type!(T)));
    define_c_type!(
        module = "CSliceMut!().h";
        #[repr(C)]
        #[derive(Clone, Copy)]
        pub struct CSliceMut!() {
            data: *mut swig_subst_type!(T),
            len: usize,
        }
    );
    foreign_code!(module = "CSliceMut!().h";
                    r##"
#ifdef __cplusplus
#include "rust_slice_tmpl.hpp"
#endif
"##);
    ($p:r_type) <T: SwigTypeIsReprC> &mut [T] => CSliceMut!() {
        $out = CSliceMut!() {
            data: $p.as_mut_ptr(),
            len: $p.len(),
        };
    };
    ($p:r_type) <T: SwigTypeIsReprC> &mut [T] <= CSliceMut!() {
        $out = unsafe { (CRustSliceMut { data: $p.data.cast(), len: $p.len }).as_slice_mut::<swig_subst_type!(T)>() };
    };
    ($p:f_type, req_modules = ["\"CSliceMut!().h\""]) => "RustSlice<swig_f_type!(T)>"
        "RustSlice<swig_f_type!(T)>{$p}";
    ($p:f_type, req_modules = ["\"CSliceMut!().h\""]) <= "RustSlice<swig_f_type!(T)>"
        "$p.as_c<CSliceMut!()>()";
);

