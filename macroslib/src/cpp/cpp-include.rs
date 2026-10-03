mod swig_foreign_types_map {}

foreign_typemap!(
    (r_type) ::std::os::raw::c_char;
    (f_type) "char";
);

foreign_typemap!(
    (r_type) ::std::os::raw::c_short;
    (f_type) "short";
);

foreign_typemap!(
    (r_type) ::std::os::raw::c_ushort;
    (f_type) "unsigned short";
);

foreign_typemap!(
    (r_type) ::std::os::raw::c_int;
    (f_type) "int";
);

foreign_typemap!(
    (r_type) ::std::os::raw::c_uint;
    (f_type) "unsigned int";
);

foreign_typemap!(
    (r_type) ::std::os::raw::c_long;
    (f_type) "long";
);

foreign_typemap!(
    (r_type) f32;
    (f_type) "float";
);

foreign_typemap!(
    (r_type) f64;
    (f_type) "double";
);

foreign_typemap!(
    (r_type) ();
    (f_type) "void";
);

foreign_typemap!(
    (r_type) i8;
    (f_type, req_modules = ["<stdint.h>"]) "int8_t";
);

foreign_typemap!(
    (r_type) u8;
    (f_type, req_modules = ["<stdint.h>"]) "uint8_t";
);

foreign_typemap!(
    (r_type) i16;
    (f_type, req_modules = ["<stdint.h>"]) "int16_t";
);

foreign_typemap!(
    (r_type) u16;
    (f_type, req_modules = ["<stdint.h>"]) "uint16_t";
);

foreign_typemap!(
    (r_type) i32;
    (f_type, req_modules = ["<stdint.h>"]) "int32_t";
);

foreign_typemap!(
    (r_type) u32;
    (f_type, req_modules = ["<stdint.h>"]) "uint32_t";
);

foreign_typemap!(
    (r_type) i64;
    (f_type, req_modules = ["<stdint.h>"]) "int64_t";
);

foreign_typemap!(
    (r_type) u64;
    (f_type, req_modules = ["<stdint.h>"]) "uint64_t";
);

foreign_typemap!(
    (r_type) usize;
    (f_type, req_modules = ["<stdint.h>"]) "uintptr_t";
);

foreign_typemap!(
    (r_type) isize;
    (f_type, req_modules = ["<stdint.h>"]) "intptr_t";
);

#[allow(unused_macros)]
macro_rules! swig_c_str {
    ($lit:expr) => {
        concat!($lit, "\0").as_ptr() as *const ::std::os::raw::c_char
    };
}

#[allow(dead_code)]
pub trait SwigForeignClass {
    fn c_class_name() -> *const ::std::os::raw::c_char;
    fn box_object(x: Self) -> *mut ::std::os::raw::c_void;
    fn unbox_object(p: *mut ::std::os::raw::c_void) -> Self;
}

#[allow(dead_code)]
pub trait SwigForeignEnum {
    fn as_u32(&self) -> u32;
    fn from_u32(_: u32) -> Self;
}

#[allow(dead_code)]
#[swig_code = "let mut {to_var}: {to_var_type} = <{to_var_type}>::swig_from({from_var});"]
trait SwigFrom<T> {
    fn swig_from(_: T) -> Self;
}

#[allow(dead_code)]
#[repr(C)]
#[derive(Clone, Copy)]
pub struct CRustStrView {
    data: *const ::std::os::raw::c_char,
    len: usize,
}

#[allow(dead_code)]
impl CRustStrView {
    fn from_str(s: &str) -> CRustStrView {
        CRustStrView {
            data: s.as_ptr() as *const ::std::os::raw::c_char,
            len: s.len(),
        }
    }
}

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

#[allow(dead_code)]
#[repr(C)]
pub struct CRustVecStringElem { _unused: u8 }

#[allow(dead_code)]
#[repr(C)]
#[derive(Clone, Copy)]
pub struct CRustVecString {
    data: *mut CRustVecStringElem,
    len: usize,
    capacity: usize,
}

#[allow(dead_code)]
impl CRustVecString {
    fn from_vec(mut value: Vec<String>) -> Self {
        let result = Self {
            data: value.as_mut_ptr().cast(),
            len: value.len(),
            capacity: value.capacity(),
        };
        ::std::mem::forget(value);
        result
    }

    unsafe fn into_vec(self) -> Vec<String> {
        unsafe { Vec::from_raw_parts(self.data.cast(), self.len, self.capacity) }
    }
}

foreign_typemap!(
    ($p:r_type) <T> Arc<Mutex<T>> => &Mutex<T> {
        $out = & $p;
    };
);

foreign_typemap!(
    ($p:r_type) <T> &Mutex<T> => MutexGuard<T> {
        $out = $p.lock().unwrap();
    };
);

foreign_typemap!(
    ($p:r_type) <T> MutexGuard<T> => &T {
        $out = & $p;
    };
);

foreign_typemap!(
    ($p:r_type) <T> MutexGuard<T> => &mut T {
        $out = &mut $p;
    };
);

foreign_typemap!(
    ($p:r_type) <T> Rc<T> => &T {
        $out = & $p;
    };
);

foreign_typemap!(
    ($p:r_type) <T> &Rc<T> => &T {
        $out = & $p;
    };
);

foreign_typemap!(
    ($p:r_type) <T> &RefCell<T> => Ref<T> {
        $out = $p.borrow();
    };
);

foreign_typemap!(
    ($p:r_type) <T> &RefCell<T> => RefMut<T> {
        $out = $p.borrow_mut();
    };
);

foreign_typemap!(
    ($p:r_type) <T> Ref<T> => &T {
        $out = & $p;
    };
);

foreign_typemap!(
    ($p:r_type) <T> RefMut<T> => &mut T {
        $out = &mut $p;
    };
);

foreign_typemap!(
    ($p:r_type) &str => &Path {
        $out = Path::new($p);
    };
);

#[allow(dead_code)]
#[repr(C)]
#[derive(Copy, Clone)]
pub struct CRustString {
    data: *mut ::std::os::raw::c_char,
    len: usize,
    capacity: usize,
}

#[allow(dead_code)]
impl CRustString {
    pub fn from_string(mut s: String) -> CRustString {
        let data = s.as_mut_ptr() as *mut ::std::os::raw::c_char;
        let len = s.len();
        let capacity = s.capacity();
        ::std::mem::forget(s);
        CRustString {
            data,
            len,
            capacity,
        }
    }
}

foreign_typemap!(
    ($p:r_type) &str => String {
        $out = String::from($p);
    };
);

impl<T: SwigForeignEnum> SwigFrom<T> for u32 {
    fn swig_from(x: T) -> u32 {
        x.as_u32()
    }
}

impl<T: SwigForeignEnum> SwigFrom<u32> for T {
    fn swig_from(x: u32) -> T {
        T::from_u32(x)
    }
}

foreign_typemap!(
    (r_type) * mut ::std::os::raw::c_void;
    (f_type) "void *";
);

foreign_typemap!(
    (r_type) * const ::std::os::raw::c_void;
    (f_type) "const void *";
);

foreign_typemap!(
    (r_type) <T: SwigTypeIsReprC> *const T;
    (f_type) "const swig_f_type!(T) *";
);

foreign_typemap!(
    (r_type) <T: SwigTypeIsReprC> *mut T;
    (f_type) "swig_f_type!(T) *";
);

// A slice of Rust objects points to their storage, not to C++ wrapper objects.
// The struct tag supplies an opaque C declaration without including the wrapper header.
foreign_typemap!(
    (r_type) <T: SwigForeignClassDirectAccess> *const T;
    (f_type) "const struct swig_f_type!(T, name_only)Opaque *";
);

foreign_typemap!(
    (r_type) <T: SwigForeignClassDirectAccess> *mut T;
    (f_type) "struct swig_f_type!(T, name_only)Opaque *";
);

foreign_typemap!(
    generic_alias!(CFnOneArgPtr = swig_concat_idents!(c_fn_, swig_i_type!(T), _t));
    foreign_code!(
        module = "CFnOneArgPtr!().h";
        r##"
typedef void (*CFnOneArgPtr!())(swig_f_type!(T));
"##
    );
    (r_type) <T: SwigTypeIsReprC> Option<extern "C" fn(T)>;
    (f_type, req_modules = ["\"CFnOneArgPtr!().h\""]) "CFnOneArgPtr!()";
);

foreign_typemap!(
    generic_alias!(CFnOneArgAndRetPtr = swig_concat_idents!(c_fn_, swig_i_type!(TArg), _ret_, swig_i_type!(TRet), _t));
    foreign_code!(
        module = "CFnOneArgAndRetPtr!().h";
                    r##"
        typedef swig_f_type!(TRet) (*CFnOneArgAndRetPtr!())(swig_f_type!(TArg));
        "##
    );
    (r_type) <TArg: SwigTypeIsReprC, TRet: SwigTypeIsReprC> Option<extern "C" fn(TArg) -> TRet>;
    (f_type, req_modules = ["\"CFnOneArgAndRetPtr!().h\""]) "CFnOneArgAndRetPtr!()";
);

foreign_typemap!(
    generic_alias!(CFnTwoArgsPtr = swig_concat_idents!(c_fn_, swig_i_type!(T1), swig_i_type!(T2), _t));
    foreign_code!(
        module = "CFnTwoArgsPtr!().h";
        r##"
typedef void (*CFnTwoArgsPtr!())(swig_f_type!(T1), swig_f_type!(T2));
"##
    );
    (r_type) <T1: SwigTypeIsReprC, T2: SwigTypeIsReprC> Option<extern "C" fn(T1, T2)>;
    (f_type, req_modules = ["\"CFnTwoArgsPtr!().h\""]) "CFnTwoArgsPtr!()";
);

foreign_typemap!(
   generic_alias!(CRustPair = swig_concat_idents!(CRustPair, swig_i_type!(T1), swig_i_type!(T2)));
   generic_alias!(CRustPairModule = swig_concat_idents!(rust_tuple, swig_i_type!(T1), swig_i_type!(T2)));
   define_c_type!(
        module = "CRustPairModule!().h";
       #[repr(C)]
       #[derive(Clone, Copy)]
       pub struct CRustPair!() {
           first: swig_i_type!(T1),
           second: swig_i_type!(T2),
       }
   );
   ($p:r_type) <T1, T2> (T1, T2) => CRustPair!() {
       swig_from_rust_to_i_type!(T1, $p.0, p0)
       swig_from_rust_to_i_type!(T2, $p.1, p1)
       $out = CRustPair!() {
           first: p0,
           second: p1,
       };
   };
   ($p:r_type) <T1, T2> (T1, T2) <= CRustPair!() {
       swig_from_i_type_to_rust!(T1, $p.first, p0)
       swig_from_i_type_to_rust!(T2, $p.second, p1)
       $out = (p0, p1);
   };
   ($p:f_type, req_modules = ["\"CRustPairModule!().h\"", "<utility>"]) => "std::pair<swig_f_type!(T1), swig_f_type!(T2)>"
           "std::make_pair(swig_foreign_from_i_type!(T1, $p.first), swig_foreign_from_i_type!(T2, $p.second))";
   ($p:f_type, req_modules = ["\"CRustPairModule!().h\"", "<utility>"]) <= "std::pair<swig_f_type!(T1), swig_f_type!(T2)>"
       "swig_f_type!(CRustPair!()) { swig_foreign_to_i_type!(T1, $p.first), swig_foreign_to_i_type!(T2, $p.second) }";
);

foreign_typemap!(
   generic_alias!(CRustTuple3 = swig_concat_idents!(CRustTuple3, swig_i_type!(T1), swig_i_type!(T2), swig_i_type!(T3)));
   generic_alias!(CRustTuple3Module = swig_concat_idents!(rust_tuple, swig_i_type!(T1), swig_i_type!(T2), swig_i_type!(T3)));
   define_c_type!(
        module = "CRustTuple3Module!().h";
       #[repr(C)]
       #[derive(Clone, Copy)]
       pub struct CRustTuple3!() {
           first: swig_i_type!(T1),
           second: swig_i_type!(T2),
           third: swig_i_type!(T3),
       }
   );
   ($p:r_type) <T1, T2, T3> (T1, T2, T3) => CRustTuple3!() {
       swig_from_rust_to_i_type!(T1, $p.0, p0)
       swig_from_rust_to_i_type!(T2, $p.1, p1)
       swig_from_rust_to_i_type!(T3, $p.2, p2)
       $out = CRustTuple3!() {
           first: p0,
           second: p1,
           third: p2,
       };
   };
   ($p:r_type) <T1, T2, T3> (T1, T2, T3) <= CRustTuple3!() {
       swig_from_i_type_to_rust!(T1, $p.first, p0)
       swig_from_i_type_to_rust!(T2, $p.second, p1)
       swig_from_i_type_to_rust!(T3, $p.third, p2)
       $out = (p0, p1, p2);
   };
   ($p:f_type, req_modules = ["\"CRustTuple3Module!().h\"", "<tuple>"]) => "std::tuple<swig_f_type!(T1), swig_f_type!(T2), swig_f_type!(T3)>"
       r#"std::make_tuple(swig_foreign_from_i_type!(T1, $p.first),
                           swig_foreign_from_i_type!(T2, $p.second),
                           swig_foreign_from_i_type!(T3, $p.third))"#;
   ($p:f_type, req_modules = ["\"CRustTuple3Module!().h\"", "<tuple>"]) <= "std::tuple<swig_f_type!(T1), swig_f_type!(T2), swig_f_type!(T3)>"
       r#"swig_f_type!(CRustTuple3!()) {
               swig_foreign_to_i_type!(T1, std::get<0>($p)),
               swig_foreign_to_i_type!(T2, std::get<1>($p)),
               swig_foreign_to_i_type!(T3, std::get<2>($p)),
        }"#;
);

foreign_typemap!(
   generic_alias!(CRustTuple4 = swig_concat_idents!(CRustTuple4, swig_i_type!(T1), swig_i_type!(T2), swig_i_type!(T3), swig_i_type!(T4)));
   generic_alias!(CRustTuple4Module = swig_concat_idents!(rust_tuple, swig_i_type!(T1), swig_i_type!(T2), swig_i_type!(T3), swig_i_type!(T4)));
   define_c_type!(
        module = "CRustTuple4Module!().h";
       #[repr(C)]
       #[derive(Clone, Copy)]
       pub struct CRustTuple4!() {
           f1: swig_i_type!(T1),
           f2: swig_i_type!(T2),
           f3: swig_i_type!(T3),
           f4: swig_i_type!(T4),
       }
   );
   ($p:r_type) <T1, T2, T3, T4> (T1, T2, T3, T4) => CRustTuple4!() {
       swig_from_rust_to_i_type!(T1, $p.0, p0)
       swig_from_rust_to_i_type!(T2, $p.1, p1)
       swig_from_rust_to_i_type!(T3, $p.2, p2)
       swig_from_rust_to_i_type!(T4, $p.3, p3)
       $out = CRustTuple4!() {
           f1: p0,
           f2: p1,
           f3: p2,
           f4: p3,
       };
   };
   ($p:r_type) <T1, T2, T3, T4> (T1, T2, T3, T4) <= CRustTuple4!() {
       swig_from_i_type_to_rust!(T1, $p.f1, p0)
       swig_from_i_type_to_rust!(T2, $p.f2, p1)
       swig_from_i_type_to_rust!(T3, $p.f3, p2)
       swig_from_i_type_to_rust!(T4, $p.f4, p3)
       $out = (p0, p1, p2, p3);
   };
   ($p:f_type, req_modules = ["\"CRustTuple4Module!().h\"", "<tuple>"]) => "std::tuple<swig_f_type!(T1), swig_f_type!(T2), swig_f_type!(T3), swig_f_type!(T4)>"
       r#"std::make_tuple(swig_foreign_from_i_type!(T1, $p.f1),
                           swig_foreign_from_i_type!(T2, $p.f2),
                           swig_foreign_from_i_type!(T3, $p.f3),
                           swig_foreign_from_i_type!(T4, $p.f4))"#;
    ($p:f_type, req_modules = ["\"CRustTuple4Module!().h\"", "<tuple>"]) <= "std::tuple<swig_f_type!(T1), swig_f_type!(T2), swig_f_type!(T3), swig_f_type!(T4)>"
       r#"swig_f_type!(CRustTuple4!()) {
               swig_foreign_to_i_type!(T1, std::get<0>($p)),
               swig_foreign_to_i_type!(T2, std::get<1>($p)),
               swig_foreign_to_i_type!(T3, std::get<2>($p)),
               swig_foreign_to_i_type!(T4, std::get<3>($p)),
        }"#;
);

foreign_typemap!(
    ($pin:r_type) bool => ::std::os::raw::c_char {
        $out = if $pin  { 1 } else { 0 };
    };
    ($pin:f_type) => "bool" "($pin != 0)";
    ($pin:r_type) bool <= ::std::os::raw::c_char {
        $out = $pin != 0;
    };
    ($pin:f_type) <= "bool" "static_cast<char>($pin ? 1 : 0)";
);

foreign_typemap!(
    define_c_type!(module = "rust_str.h";
        #[repr(C)]
        #[derive(Clone, Copy)]
        pub struct CRustStrView {
            data: *const ::std::os::raw::c_char,
            len: usize,
        }
    );
    ($p:r_type) &str => CRustStrView {
        $out = CRustStrView::from_str($p);
    };
    ($p:r_type) &str <= CRustStrView {
        $out = if $p.len == 0 {
            ""
        } else {
            unsafe {
                let slice: &[u8] = ::std::slice::from_raw_parts($p.data as *const u8, $p.len);
                ::std::str::from_utf8_unchecked(slice)
            }
        };
    };

    ($p:f_type, option = "CppStrView::Boost", req_modules = ["\"rust_str.h\"", "<boost/utility/string_view.hpp>"]) => "boost::string_view"
        "boost::string_view{ $p.data, $p.len }";
    ($p:f_type, option = "CppStrView::Boost", req_modules = ["\"rust_str.h\"", "<boost/utility/string_view.hpp>"]) <= "boost::string_view"
        "CRustStrView{ $p.data(), $p.size() }";

    ($p:f_type, option = "CppStrView::Std17", req_modules = ["\"rust_str.h\"", "<string_view>"]) => "std::string_view"
        "std::string_view{ $p.data, $p.len }";
    ($p:f_type, option = "CppStrView::Std17", req_modules = ["\"rust_str.h\"", "<string_view>"]) <= "std::string_view"
        "CRustStrView{ $p.data(), $p.size() }";
);

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

foreign_typemap!(
    define_c_type!(module = "rust_str.h";
        #[repr(C)]
        #[derive(Clone, Copy)]
        struct CRustString {
            data: *mut ::std::os::raw::c_char,
            len: usize,
            capacity: usize,
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn crust_string_free(x: CRustString) {
            let s = unsafe { String::from_raw_parts(x.data.cast(), x.len, x.capacity) };
            drop(s);
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn crust_string_clone(x: CRustString) -> CRustString {
            let s = unsafe { String::from_raw_parts(x.data.cast(), x.len, x.capacity) };
            let ret = CRustString::from_string(s.clone());
            ::std::mem::forget(s);
            ret
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn crust_string_push_str(x: CRustString, s: CRustStrView) -> CRustString {
            let mut x = unsafe { String::from_raw_parts(x.data.cast(), x.len, x.capacity) };
            let addon: &str = if s.len == 0 {
                ""
            } else {
                unsafe {
                    let slice: &[u8] = ::std::slice::from_raw_parts(s.data as *const u8, s.len);
                    ::std::str::from_utf8_unchecked(slice)
                }
            };
            x.push_str(addon);
            CRustString::from_string(x)
        }
    );
    foreign_code!(module = "rust_str.h";
                    r##"
#ifdef __cplusplus

#include <string>
"##
    );
    foreign_code!(module = "rust_str.h";
                    option = "CppStrView::Std17";
                    r##"
#include <string_view>
"##);
    foreign_code!(module = "rust_str.h";
                    option = "CppStrView::Boost";
                    r##"
#include <boost/utility/string_view.hpp>
"##);
    foreign_code!(module = "rust_str.h";
                    r##"
namespace $RUST_SWIG_USER_NAMESPACE {
class RustString final : private CRustString {
public:
    explicit RustString(const CRustString &o) noexcept
    {
        data = o.data;
        len = o.len;
        capacity = o.capacity;
    }
    RustString() noexcept { reset(*this); }
    RustString(const RustString &o) noexcept
        : RustString(crust_string_clone(o))
    {
    }
    RustString &operator=(const RustString &o) noexcept
    {
        if (this != &o) {
            free_mem();
            auto copy = crust_string_clone(o);
            data = copy.data;
            len = copy.len;
            capacity = copy.capacity;
        }
        return *this;
    }
    RustString(RustString &&o) noexcept
    {
        data = o.data;
        len = o.len;
        capacity = o.capacity;

        reset(o);
    }
    RustString &operator=(RustString &&o) noexcept
    {
        free_mem();
        data = o.data;
        len = o.len;
        capacity = o.capacity;

        reset(o);
        return *this;
    }
    ~RustString() noexcept { free_mem(); }
    std::string to_std_string() const { return std::string(data, len); }
    size_t size() const noexcept { return this->len; }
    bool empty() const noexcept { return this->len == 0; }
    CRustString release() noexcept
    {
        CRustString ret{ this->data, this->len, this->capacity };
        reset(*this);
        return ret;
    }
"##);
    foreign_code!(module = "rust_str.h";
                    option = "CppStrView::Std17";
                    r##"
    using CppStringViewT = std::string_view;
    std::string_view to_string_view() const { return std::string_view(data, len); }
    bool operator==(const RustString& o) const noexcept {
        return to_string_view() == o.to_string_view();
    }
"##);
    foreign_code!(module = "rust_str.h";
                    option = "CppStrView::Boost";
                    r#"
    using CppStringViewT = boost::string_view;
    boost::string_view to_boost_string_view() const { return boost::string_view{ data, len }; }
    bool operator==(const RustString& o) const noexcept {
        return to_boost_string_view() == o.to_boost_string_view();
    }
"#);
    foreign_code!(module = "rust_str.h";
                    r##"
    void push_str(CppStringViewT s) noexcept {
        auto new_cstr = crust_string_push_str(*this, CRustStrView{ s.data(), s.size() });
        data = new_cstr.data;
        len = new_cstr.len;
        capacity = new_cstr.capacity;
    }
    explicit RustString(CppStringViewT s) noexcept {
        reset(*this);
        push_str(s);
    }
    RustString& operator+=(CppStringViewT s) noexcept {
        push_str(s);
        return *this;
    }
private:
    //analog of Rust's NonNull::danging, can not use C cast (and/or reinterpret_cast)
    // in constexpr, so macros
    #define DANGLING  reinterpret_cast<char *>(alignof(char))
    void free_mem() noexcept
    {
        if (data != DANGLING) {
            crust_string_free(*this);
            reset(*this);
        }
    }
    static void reset(RustString &o) noexcept
    {
        o.data = DANGLING;
        o.len = 0;
        o.capacity = 0;
    }
    #undef DANGLING
};
} // namespace $RUST_SWIG_USER_NAMESPACE
#endif // __cplusplus
"##
                    );
    ($pin:r_type) String => CRustString {
        $out = CRustString::from_string($pin);
    };
    ($pin:f_type, req_modules = ["\"rust_str.h\""]) => "RustString" "RustString{$pin}";

    ($pin:r_type) String <= CRustString {
        $out = unsafe { String::from_raw_parts($pin.data.cast(), $pin.len, $pin.capacity) };
    };
    ($pin:f_type, req_modules = ["\"rust_str.h\""]) <= "RustString" "$pin.release()";
);

foreign_typemap!(
    generic_alias!(CRustClassOpt = swig_concat_idents!(CRustClassOpt, swig_f_type!(T)));
    define_c_type!(
         module = "rust_option.h";
         #[repr(C)]
         #[derive(Clone, Copy)]
         pub struct CRustClassOpt!() {
             p: *const ::std::os::raw::c_void,
         }
    );
    ($p:r_type) <T: SwigForeignClass> Option<&T> <= CRustClassOpt!() {
        $out = if !$p.p.is_null() {
            swig_from_i_type_to_rust!(&T, $p.p, obj)
            Some(obj)
        } else {
            None
        };
    };
    ($p:f_type, unique_prefix = "/*opt ref*/", req_modules = ["\"rust_option.h\""]) <= "/*opt ref*/const swig_f_type!(T) *" r#"
        $out = CRustClassOpt!() { ($p != nullptr) ? static_cast<swig_f_type!(T)Opaque *>(* $p) : nullptr };
"#;
);

foreign_typemap!(
    generic_alias!(CRustClassOptMut = swig_concat_idents!(CRustClassOptMut, swig_f_type!(T)));
    define_c_type!(
         module = "rust_option.h";
         #[repr(C)]
         #[derive(Clone, Copy)]
         pub struct CRustClassOptMut!() {
             p: *mut ::std::os::raw::c_void,
         }
    );
    ($p:r_type) <T: SwigForeignClass> Option<&mut T> <= CRustClassOptMut!() {
        $out = if !$p.p.is_null() {
            swig_from_i_type_to_rust!(&mut T, $p.p, obj)
            Some(obj)
        } else {
            None
        };
    };
    ($p:f_type, unique_prefix = "/*opt mut ref*/", req_modules = ["\"rust_option.h\""]) <= "/*opt mut ref*/swig_f_type!(T) *" r#"
        $out = CRustClassOptMut!() { ($p != nullptr) ? static_cast<swig_f_type!(T)Opaque *>(* $p) : nullptr };
"#;
);

foreign_typemap!(
    generic_alias!(CRustOpt = swig_concat_idents!(CRustOption, swig_i_type!(T)));
    generic_alias!(CRustOptUnion = swig_concat_idents!(CRustOptionUnion, swig_i_type!(T)));
    define_c_type!(
         module = "rust_option.h";
         #[repr(C)]
         #[derive(Clone, Copy)]
         pub union CRustOptUnion!() {
             data: swig_i_type!(T),
             uninit: u8,
         }

         #[repr(C)]
         #[derive(Clone, Copy)]
         pub struct CRustOpt!() {
             val: CRustOptUnion!(),
             is_some: u8,
         }
     );
    ($p:r_type) <T> Option<T> => CRustOpt!() {
        $out = match $p {
            Some(mut x) => {
                swig_from_rust_to_i_type!(T, x, data)
                CRustOpt!() {
                    val: CRustOptUnion!() { data },
                    is_some: 1,
                }
            }
            None => CRustOpt!() {
                    val: CRustOptUnion!() { uninit: 0 },
                    is_some: 0,
            },
        };
    };
    ($p:r_type) <T> Option<T> <= CRustOpt!() {
        $out = if $p.is_some != 0 {
            swig_from_i_type_to_rust!(T, unsafe { $p.val.data }, ret)
            Some(ret)
        } else {
            None
        };
    };

    ($p:f_type, option = "CppOptional::Boost", req_modules = ["\"rust_option.h\"", "<boost/optional.hpp>"]) => "boost::optional<swig_f_type!(T)>"
        "($p.is_some != 0) ? boost::optional<swig_f_type!(T)>(swig_foreign_from_i_type!(T, $p.val.data)) : boost::optional<swig_f_type!(T)>()";
    ($p:f_type, option = "CppOptional::Boost", req_modules = ["\"rust_option.h\"", "<boost/optional.hpp>"]) <= "boost::optional<swig_f_type!(T)>"
        r#"[](boost::optional<swig_f_type!(T)> p) -> CRustOpt!() {
            CRustOpt!() out;
            if (!!p) {
                out.val.data = swig_foreign_to_i_type!(T, (*p));
                out.is_some = 1;
            } else {
                out.is_some = 0;
            }
            return out;
            }(std::move($p))"#;

    ($p:f_type, option = "CppOptional::Std17", req_modules = ["\"rust_option.h\"", "<optional>"]) => "std::optional<swig_f_type!(T)>"
        "($p.is_some != 0) ? std::optional<swig_f_type!(T)>(swig_foreign_from_i_type!(T, $p.val.data)) : std::optional<swig_f_type!(T)>()";
    ($p:f_type, option = "CppOptional::Std17", req_modules = ["\"rust_option.h\"", "<optional>"]) <= "std::optional<swig_f_type!(T)>"
        r#"[](std::optional<swig_f_type!(T)> p) -> CRustOpt!() {
            CRustOpt!() out;
            if (p.has_value()) {
                out.val.data = swig_foreign_to_i_type!(T, (*p));
                out.is_some = 1;
            } else {
                out.is_some = 0;
            }
            return out;
            }(std::move($p))"#;
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

#[allow(dead_code)]
#[repr(C)]
#[derive(Copy, Clone)]
pub struct CRustVecAccess {
    data: *mut ::std::os::raw::c_void,
    len: usize,
    capacity: usize,
}

#[allow(dead_code)]
impl CRustVecAccess {
    pub fn from_vec<T>(mut v: Vec<T>) -> Self {
        let data = v.as_mut_ptr() as *mut ::std::os::raw::c_void;
        let len = v.len();
        let capacity = v.capacity();
        ::std::mem::forget(v);
        Self {
            data,
            len,
            capacity,
        }
    }
    pub fn to_slice<'a, T>(cs: Self) -> &'a [T] {
        if cs.len == 0 {
            &[]
        } else {
            unsafe { ::std::slice::from_raw_parts(cs.data as *const T, cs.len) }
        }
    }
    pub fn to_vec<T>(cs: Self) -> Vec<T> {
        unsafe { Vec::from_raw_parts(cs.data.cast(), cs.len, cs.capacity) }
    }
}

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

foreign_typemap!(
    generic_alias!(CRustVec = swig_concat_idents!(CRustVec, swig_i_type!(T)));
    generic_alias!(CRustVecModule = swig_concat_idents!(rust_vec_, swig_i_type!(T)));
    generic_alias!(CRustVecFree = swig_concat_idents!(CRustVec, swig_i_type!(T), _free));
    generic_alias!(CppRustVec = swig_concat_idents!(RustVec, swig_i_type!(T)));
    define_c_type!(
        module = "CRustVecModule!().h";
        #[repr(C)]
        #[derive(Copy, Clone)]
        pub struct CRustVec!() {
            data: *mut swig_subst_type!(T),
            len: usize,
            capacity: usize,
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn CRustVecFree!()(v: CRustVec!()) {
            let v: Vec<swig_subst_type!(T)> = unsafe { Vec::from_raw_parts(v.data, v.len, v.capacity) };
            drop(v);
        }
    );
    foreign_code!(module = "CRustVecModule!().h";
                    r##"
#ifdef __cplusplus

#include "rust_vec_impl.hpp"

namespace $RUST_SWIG_USER_NAMESPACE {
using CppRustVec!() = RustVec<CRustVec!(), internal::NativeVecPolicy<CRustVec!(), CRustVecFree!()>>;
}

#endif
"##);
    ($p:r_type) <T: SwigTypeIsReprC> Vec<T> => CRustVec!() {
        let mut tmp = $p;
        let p = tmp.as_mut_ptr();
        let len = tmp.len();
        let cap = tmp.capacity();
        ::std::mem::forget(tmp);
        $out = CRustVec!() {
            data: p,
            len,
            capacity: cap,
        };
    };
    ($p:f_type, req_modules = ["\"CRustVecModule!().h\""]) => "CppRustVec!()"
        "CppRustVec!(){$p}";
    ($p:r_type) <T: SwigTypeIsReprC> Vec<T> <= CRustVec!() {
        $out = unsafe { Vec::from_raw_parts($p.data, $p.len, $p.capacity) };
    };
    ($p:f_type, req_modules = ["\"CRustVecModule!().h\""]) <= "CppRustVec!()"
        "$p.release()";
);

foreign_typemap!(
    foreign_code!(module = "rust_vec_string.h";
                    r##"
#include "rust_str.h"
typedef struct CRustVecStringElem CRustVecStringElem;
typedef struct CRustVecString CRustVecString;
"##);
);

foreign_typemap!(
    define_c_type!(module = "rust_vec_string.h";
        #[repr(C)]
        pub struct CRustVecStringElem { _unused: u8 }
        #[repr(C)]
        #[derive(Clone, Copy)]
        pub struct CRustVecString {
            data: *mut CRustVecStringElem,
            len: usize,
            capacity: usize,
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn crust_vec_string_new() -> CRustVecString {
            CRustVecString::from_vec(Vec::new())
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn crust_vec_string_free(value: CRustVecString) {
            drop(unsafe { value.into_vec() });
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn crust_vec_string_get(value: CRustVecString, index: usize) -> CRustStrView {
            let values = unsafe { (CRustSlice { data: value.data.cast(), len: value.len }).as_slice::<String>() };
            CRustStrView::from_str(values[index].as_str())
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn crust_vec_string_push(value: *mut CRustVecString, item: CRustString) {
            let value = unsafe { &mut *value };
            let mut values = unsafe { (*value).into_vec() };
            values.push(unsafe { String::from_raw_parts(item.data.cast(), item.len, item.capacity) });
            *value = CRustVecString::from_vec(values);
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn crust_vec_string_remove(value: *mut CRustVecString, index: usize) -> CRustString {
            let value = unsafe { &mut *value };
            let mut values = unsafe { (*value).into_vec() };
            let item = values.remove(index);
            *value = CRustVecString::from_vec(values);
            CRustString::from_string(item)
        }
    );
    foreign_code!(module = "rust_vec_string.h";
                    r##"
#ifdef __cplusplus
#include "rust_vec_impl.hpp"
#include "rust_str.h"

namespace $RUST_SWIG_USER_NAMESPACE {
namespace internal {
template <typename View> struct StringVecPolicy {
    using value_type = RustString;
    using reference = View;
    using iterator = SliceIterator<CRustVecString, StringVecPolicy<View>>;
    using const_iterator = iterator;

    static CRustVecString empty() noexcept { return crust_vec_string_new(); }
    static void free(CRustVecString vec) noexcept { crust_vec_string_free(vec); }
    static View index(CRustVecString vec, size_t i) noexcept
    {
        const auto str = crust_vec_string_get(vec, i);
        return str.len == 0 ? View{} : View{ str.data, str.len };
    }
    static iterator begin(CRustVecString vec) noexcept { return iterator{ vec, 0 }; }
    static const_iterator cbegin(CRustVecString vec) noexcept { return begin(vec); }
    static iterator end(CRustVecString vec) noexcept { return iterator{ vec, vec.len }; }
    static const_iterator cend(CRustVecString vec) noexcept { return end(vec); }
    static void push(CRustVecString &vec, RustString value) noexcept
    {
        crust_vec_string_push(&vec, value.release());
    }
    static RustString remove(CRustVecString &vec, size_t i) noexcept
    {
        assert(i < vec.len);
        return RustString{ crust_vec_string_remove(&vec, i) };
    }
};
} // namespace internal
} // namespace $RUST_SWIG_USER_NAMESPACE
#endif
"##);
    foreign_code!(module = "rust_vec_string.h";
                    option = "CppStrView::Std17";
                    r##"
#ifdef __cplusplus
namespace $RUST_SWIG_USER_NAMESPACE {
using RustVecString = RustVec<CRustVecString, internal::StringVecPolicy<std::string_view>>;
}
#endif
"##);
    foreign_code!(module = "rust_vec_string.h";
                    option = "CppStrView::Boost";
                    r##"
#ifdef __cplusplus
namespace $RUST_SWIG_USER_NAMESPACE {
using RustVecString = RustVec<CRustVecString, internal::StringVecPolicy<boost::string_view>>;
}
#endif
"##);
    ($p:r_type) Vec<String> => CRustVecString {
        $out = CRustVecString::from_vec($p);
    };
    ($p:r_type) Vec<String> <= CRustVecString {
        $out = unsafe { $p.into_vec() };
    };
    ($p:f_type, req_modules = ["\"rust_vec_string.h\""]) => "RustVecString"
        "RustVecString{$p}";
    ($p:f_type, req_modules = ["\"rust_vec_string.h\""]) <= "RustVecString"
        "$p.release()";
);

#[allow(dead_code)]
#[repr(C)]
#[derive(Copy, Clone)]
pub struct CRustForeignVec {
    data: *mut ::std::os::raw::c_void,
    len: usize,
    capacity: usize,
}

#[allow(dead_code)]
impl CRustForeignVec {
    pub fn from_vec<T: SwigForeignClass>(mut v: Vec<T>) -> CRustForeignVec {
        let data = v.as_mut_ptr() as *mut ::std::os::raw::c_void;
        let len = v.len();
        let capacity = v.capacity();
        ::std::mem::forget(v);
        CRustForeignVec {
            data,
            len,
            capacity,
        }
    }
}

#[allow(dead_code)]
#[inline]
fn push_foreign_class_to_vec<T: SwigForeignClass>(
    vec: *mut CRustForeignVec,
    elem: *mut ::std::os::raw::c_void,
) {
    assert!(!vec.is_null());
    let vec: &mut CRustForeignVec = unsafe { &mut *vec };
    let mut v = unsafe { Vec::from_raw_parts(vec.data.cast(), vec.len, vec.capacity) };
    v.push(T::unbox_object(elem));
    vec.data = v.as_mut_ptr() as *mut ::std::os::raw::c_void;
    vec.len = v.len();
    vec.capacity = v.capacity();
    ::std::mem::forget(v);
}

#[allow(dead_code)]
#[inline]
fn remove_foreign_class_from_vec<T: SwigForeignClass>(
    vec: *mut CRustForeignVec,
    index: usize,
) -> *mut ::std::os::raw::c_void {
    assert!(!vec.is_null());
    let vec: &mut CRustForeignVec = unsafe { &mut *vec };
    let mut v = unsafe { Vec::from_raw_parts(vec.data.cast(), vec.len, vec.capacity) };
    let elem: T = v.remove(index);
    vec.data = v.as_mut_ptr() as *mut ::std::os::raw::c_void;
    vec.len = v.len();
    vec.capacity = v.capacity();
    ::std::mem::forget(v);
    T::box_object(elem)
}

#[allow(dead_code)]
#[inline]
fn drop_foreign_class_vec<T: SwigForeignClass>(v: CRustForeignVec) {
    let v = unsafe { Vec::from_raw_parts(v.data.cast::<T>(), v.len, v.capacity) };
    drop(v);
}

foreign_typemap!(
    generic_alias!(CForeignVecModule = swig_concat_idents!(RustForeignVec, swig_f_type!(T)));
    generic_alias!(CForeignVec = swig_concat_idents!(CRustForeignVec, swig_f_type!(T)));
    generic_alias!(CForeignVecNew = swig_concat_idents!(RustForeignVec, swig_f_type!(T), _new));
    generic_alias!(CForeignVecFree = swig_concat_idents!(RustForeignVec, swig_f_type!(T), _free));
    generic_alias!(CForeignVecPush = swig_concat_idents!(RustForeignVec, swig_f_type!(T), _push));
    generic_alias!(CForeignVecRemove = swig_concat_idents!(RustForeignVec, swig_f_type!(T), _remove));

    define_c_type!(
        module = "CForeignVecModule!().h";
        #[repr(C)]
        #[derive(Clone, Copy)]
        pub struct CForeignVec!() {
            data: *mut ::std::os::raw::c_void,
            len: usize,
            capacity: usize,
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn CForeignVecNew!()() -> CForeignVec!() {
            let mut v = Vec::<swig_subst_type!(T)>::new();
            CForeignVec!() {
                data: v.as_mut_ptr().cast(),
                len: 0,
                capacity: 0,
            }
        }

        #[allow(unused_variables, unused_mut, non_snake_case, unused_unsafe)]
        #[unsafe(no_mangle)]
        pub extern "C" fn CForeignVecFree!()(v: CForeignVec!()) {
            drop_foreign_class_vec::<swig_subst_type!(T)>(CRustForeignVec {
                data: v.data.cast(), len: v.len, capacity: v.capacity,
            });
        }

        #[allow(unused_variables, unused_mut, non_snake_case, unused_unsafe)]
        #[unsafe(no_mangle)]
        pub extern "C" fn CForeignVecPush!()(v: *mut CForeignVec!(), e: *mut ::std::os::raw::c_void) {
            let v = unsafe { &mut *v };
            let mut raw = CRustForeignVec {
                data: v.data.cast(), len: v.len, capacity: v.capacity,
            };
            push_foreign_class_to_vec::<swig_subst_type!(T)>(&mut raw, e);
            v.data = raw.data.cast();
            v.len = raw.len;
            v.capacity = raw.capacity;
        }

        #[allow(unused_variables, unused_mut, non_snake_case, unused_unsafe)]
        #[unsafe(no_mangle)]
        pub extern "C" fn CForeignVecRemove!()(v: *mut CForeignVec!(), idx: usize) -> *mut ::std::os::raw::c_void {
            let v = unsafe { &mut *v };
            let mut raw = CRustForeignVec {
                data: v.data.cast(), len: v.len, capacity: v.capacity,
            };
            let elem = remove_foreign_class_from_vec::<swig_subst_type!(T)>(&mut raw, idx);
            v.data = raw.data.cast();
            v.len = raw.len;
            v.capacity = raw.capacity;
            elem
        }
    );

    foreign_code!(module = "CForeignVecModule!().h";
                    r##"
#ifdef __cplusplus

#include "rust_vec_impl.hpp"

namespace $RUST_SWIG_USER_NAMESPACE {
using CForeignVecModule!() = RustVec<CForeignVec!(), internal::ForeignVecPolicy<swig_f_type!(&T, output), CForeignVec!(), CForeignVecNew!(), CForeignVecFree!(), CForeignVecPush!(), CForeignVecRemove!()>>;
}
#endif
"##);

    ($p:r_type) <T: SwigForeignClass> Vec<T> => CForeignVec!() {
        let mut v: Vec<swig_subst_type!(T)> = $p;
        $out = CForeignVec!() {
            data: v.as_mut_ptr().cast(), len: v.len(), capacity: v.capacity(),
        };
        ::std::mem::forget(v);
    };
    ($p:r_type) <T: SwigForeignClass> Vec<T> <= CForeignVec!() {
        $out = unsafe { Vec::from_raw_parts($p.data.cast(), $p.len, $p.capacity) };
    };
    ($p:f_type, req_modules = ["\"CForeignVecModule!().h\""]) => "CForeignVecModule!()"
        "CForeignVecModule!(){$p}";
    ($p:f_type, req_modules = ["\"CForeignVecModule!().h\""]) <= "CForeignVecModule!()"
        "$p.release()";
);

// order is important!!!
// we map () to void, but C++ can not handle std::variant<void,..>
foreign_typemap!(
   generic_alias!(CRustRes = swig_concat_idents!(CRustVoidOkResult, swig_i_type!(T)));
   generic_alias!(CRustResUnion = swig_concat_idents!(CRustVoidOkResultUnion, swig_i_type!(T)));
   generic_alias!(CRustResModule = swig_concat_idents!(rust_void_ok_result, swig_i_type!(T)));
   define_c_type!(
       module = "CRustResModule!().h";
       #[repr(C)]
       #[derive(Clone, Copy)]
       pub union CRustResUnion!() {
           ok: u8,
           err: swig_i_type!(T),
       }

       #[repr(C)]
       #[derive(Clone, Copy)]
       pub struct CRustRes!() {
           data: CRustResUnion!(),
           is_ok: u8,
       }
   );
   ($p:r_type) <T> Result<(), T> => CRustRes!() {
       $out = match $p {
           Ok(()) => {
               CRustRes!() {
                   data: CRustResUnion!() { ok: 0 },
                   is_ok: 1,
               }
           }
           Err(err) => {
               swig_from_rust_to_i_type!(T, err, err)
               CRustRes!() {
                   data: CRustResUnion!() { err },
                   is_ok: 0,
               }
           }
       };
   };

    ($p:f_type, option = "CppVariant::Boost",
     req_modules = ["\"CRustResModule!().h\"", "<boost/optional.hpp>"],
     unique_prefix = "/*res_empty*/") => "/*res_empty*/boost::optional<swig_f_type!(T)>"
       r#"$p.is_ok != 0 ?
            boost::none :
            boost::optional<swig_f_type!(T)> { swig_foreign_from_i_type!(T, $p.data.err) }"#;

    ($p:f_type, option = "CppVariant::Std17",
     req_modules = ["\"CRustResModule!().h\"", "<optional>"],
     unique_prefix = "/*res_empty*/") => "/*res_empty*/std::optional<swig_f_type!(T)>"
       r#"$p.is_ok != 0 ?
            std::nullopt :
            std::optional<swig_f_type!(T)> { swig_foreign_from_i_type!(T, $p.data.err) }"#;

    ($p:r_type) <T> Result<(), T> <= CRustRes!() {
       $out = unsafe {
           if $p.is_ok != 0 {
               Ok(())
           } else {
               swig_from_i_type_to_rust!(T, $p.data.err, x)
               Err(x)
           }
       };
   };

   ($p:f_type, option = "CppVariant::Boost",
    req_modules = ["\"CRustResModule!().h\"", "<boost/optional.hpp>",
                   "<cassert>", "<utility>"],
    unique_prefix = "/*res_empty*/")
       <= "/*res_empty*/boost::optional<swig_f_type!(T)>"
       r#"[](boost::optional<swig_f_type!(T)> p) -> CRustRes!() {
            CRustRes!() out;
            if (!p) {
                out.is_ok = 1;
            } else {
                swig_f_type!(T) tmp = std::move(*p);
                out.data.err = swig_foreign_to_i_type!(T, tmp);
                out.is_ok = 0;
            }
            return out;
           }(std::move($p))"#;

   ($p:f_type, option = "CppVariant::Std17",
    req_modules = ["\"CRustResModule!().h\"", "<optional>"],
    unique_prefix = "/*res_empty*/")
       <= "/*res_empty*/std::optional<swig_f_type!(T)>"
       r#"[](std::optional<swig_f_type!(T)> p) -> CRustRes!() {
            CRustRes!() out;
            if (!p) {
                out.is_ok = 1;
            } else {
                swig_f_type!(T) tmp = std::move(*p);
                out.data.err = swig_foreign_to_i_type!(T, tmp);
                out.is_ok = 0;
            }
            return out;
          }(std::move($p))"#;
);

foreign_typemap!(
   generic_alias!(CRustRes = swig_concat_idents!(CRustResult, swig_i_type!(T1), swig_i_type!(T2)));
   generic_alias!(CRustResUnion = swig_concat_idents!(CRustResultUnion, swig_i_type!(T1), swig_i_type!(T2)));
   generic_alias!(CRustResModule = swig_concat_idents!(rust_result, swig_i_type!(T1), swig_i_type!(T2)));
   define_c_type!(
        module = "CRustResModule!().h";
       #[repr(C)]
       #[derive(Clone, Copy)]
       pub union CRustResUnion!() {
           ok: swig_i_type!(T1),
           err: swig_i_type!(T2),
       }

       #[repr(C)]
       #[derive(Clone, Copy)]
       pub struct CRustRes!() {
           data: CRustResUnion!(),
           is_ok: u8,
       }
   );
   ($p:r_type) <T1, T2> Result<T1, T2> => CRustRes!() {
       $out = match $p {
           Ok(mut x) => {
               swig_from_rust_to_i_type!(T1, x, ok)
               CRustRes!() {
                   data: CRustResUnion!() { ok },
                   is_ok: 1,
               }
           }
           Err(err) => {
               swig_from_rust_to_i_type!(T2, err, err)
               CRustRes!() {
                   data: CRustResUnion!() { err },
                   is_ok: 0,
               }
           }
       };
   };

   ($p:f_type, option = "CppVariant::Boost", req_modules = ["\"CRustResModule!().h\"", "<boost/variant.hpp>"]) => "boost::variant<swig_f_type!(T1), swig_f_type!(T2)>"
       r#"$p.is_ok != 0 ?
              boost::variant<swig_f_type!(T1), swig_f_type!(T2)> { swig_foreign_from_i_type!(T1, $p.data.ok) } :
              boost::variant<swig_f_type!(T1), swig_f_type!(T2)> { swig_foreign_from_i_type!(T2, $p.data.err) }"#;

   ($p:f_type, option = "CppVariant::Std17", req_modules = ["\"CRustResModule!().h\"", "<variant>"]) => "std::variant<swig_f_type!(T1), swig_f_type!(T2)>"
       r#"$p.is_ok != 0 ?
              std::variant<swig_f_type!(T1), swig_f_type!(T2)> { swig_foreign_from_i_type!(T1, $p.data.ok) } :
              std::variant<swig_f_type!(T1), swig_f_type!(T2)> { swig_foreign_from_i_type!(T2, $p.data.err) }"#;

   ($p:r_type) <T1, T2> Result<T1, T2> <= CRustRes!() {
       $out = unsafe {
           if $p.is_ok != 0 {
               swig_from_i_type_to_rust!(T1, $p.data.ok, x)
               Ok(x)
           } else {
               swig_from_i_type_to_rust!(T2, $p.data.err, x)
               Err(x)
           }
       };
   };

   ($p:f_type, option = "CppVariant::Boost",
    req_modules = ["\"CRustResModule!().h\"", "<boost/variant.hpp>", "<cassert>"])
       <= "boost::variant<swig_f_type!(T1), swig_f_type!(T2)>"
       r#"[](boost::variant<swig_f_type!(T1), swig_f_type!(T2)> p) -> CRustRes!() {
            CRustRes!() out;
            if (boost::get<swig_f_type!(T1)>(&p) != nullptr) {
                swig_f_type!(T1) tmp = boost::get<swig_f_type!(T1)>(std::move(p));
                out.data.ok = swig_foreign_to_i_type!(T1, tmp);
                out.is_ok = 1;
            } else {
                assert(boost::get<swig_f_type!(T2)>(&p) != nullptr);
                swig_f_type!(T2) tmp = boost::get<swig_f_type!(T2)>(std::move(p));
                out.data.err = swig_foreign_to_i_type!(T2, tmp);
                out.is_ok = 0;
            }
            return out;
         }(std::move($p))"#;

   ($p:f_type, option = "CppVariant::Std17",
    req_modules = ["\"CRustResModule!().h\"", "<variant>"])
       <= "std::variant<swig_f_type!(T1), swig_f_type!(T2)>"
       r#"[](std::variant<swig_f_type!(T1), swig_f_type!(T2)> p) -> CRustRes!() {
            CRustRes!() out;
            if (std::get_if<swig_f_type!(T1)>(&p) != nullptr) {
                swig_f_type!(T1) tmp = std::get<swig_f_type!(T1)>(std::move(p));
                out.data.ok = swig_foreign_to_i_type!(T1, tmp);
                out.is_ok = 1;
            } else {
                assert(std::get_if<swig_f_type!(T2)>(&p) != nullptr);
                swig_f_type!(T2) tmp = std::get<swig_f_type!(T2)>(std::move(p));
                out.data.err = swig_foreign_to_i_type!(T2, tmp);
                out.is_ok = 0;
            }
            return out;
          }(std::move($p))"#;
);
