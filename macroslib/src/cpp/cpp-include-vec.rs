#[allow(dead_code)]
#[repr(C)]
pub struct CRustVecStringElem {
    _unused: u8,
}

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
#[derive(Copy, Clone)]
pub struct CRustForeignVec<T> {
    data: *mut T,
    len: usize,
    capacity: usize,
}

#[allow(dead_code)]
impl<T> CRustForeignVec<T> {
    pub fn from_vec(mut v: Vec<T>) -> Self {
        let data = v.as_mut_ptr();
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
    vec: *mut CRustForeignVec<T>,
    elem: *mut ::std::os::raw::c_void,
) {
    assert!(!vec.is_null());
    let vec: &mut CRustForeignVec<T> = unsafe { &mut *vec };
    let mut v = unsafe { Vec::from_raw_parts(vec.data, vec.len, vec.capacity) };
    v.push(T::unbox_object(elem));
    vec.data = v.as_mut_ptr();
    vec.len = v.len();
    vec.capacity = v.capacity();
    ::std::mem::forget(v);
}

#[allow(dead_code)]
#[inline]
fn remove_foreign_class_from_vec<T: SwigForeignClass>(
    vec: *mut CRustForeignVec<T>,
    index: usize,
) -> *mut ::std::os::raw::c_void {
    assert!(!vec.is_null());
    let vec: &mut CRustForeignVec<T> = unsafe { &mut *vec };
    let mut v = unsafe { Vec::from_raw_parts(vec.data, vec.len, vec.capacity) };
    let elem: T = v.remove(index);
    vec.data = v.as_mut_ptr();
    vec.len = v.len();
    vec.capacity = v.capacity();
    ::std::mem::forget(v);
    T::box_object(elem)
}

#[allow(dead_code)]
#[inline]
fn drop_foreign_class_vec<T: SwigForeignClass>(v: CRustForeignVec<T>) {
    let v = unsafe { Vec::from_raw_parts(v.data, v.len, v.capacity) };
    drop(v);
}

foreign_typemap!(
    generic_alias!(CForeignVecModule = swig_concat_idents!(RustForeignVec, swig_f_type!(T)));
    generic_alias!(CForeignVec = swig_concat_idents!(CRustForeignVec, swig_f_type!(T)));
    generic_alias!(CForeignVecElem = swig_concat_idents!(CRustForeignVec, swig_f_type!(T), Elem));
    generic_alias!(CForeignVecNew = swig_concat_idents!(RustForeignVec, swig_f_type!(T), _new));
    generic_alias!(CForeignVecFree = swig_concat_idents!(RustForeignVec, swig_f_type!(T), _free));
    generic_alias!(CForeignVecPush = swig_concat_idents!(RustForeignVec, swig_f_type!(T), _push));
    generic_alias!(CForeignVecRemove = swig_concat_idents!(RustForeignVec, swig_f_type!(T), _remove));
    generic_alias!(CForeignVecCloneAt = swig_concat_idents!(RustForeignVec, swig_f_type!(T), _clone_at));
    generic_alias!(CIndirectSlice = swig_concat_idents!(CRustSliceForeignIndirect, swig_f_type!(T)));
    generic_alias!(CIndirectSliceAccess = swig_concat_idents!(swig_f_type!(T), Access));

    define_c_type!(
        module = "CForeignVecModule!().h";
        #[repr(C)]
        pub struct CForeignVecElem!() { _unused: u8 }

        #[repr(C)]
        #[derive(Clone, Copy)]
        pub struct CForeignVec!() {
            data: *mut CForeignVecElem!(),
            len: usize,
            capacity: usize,
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn CForeignVecNew!()() -> CForeignVec!() {
            let raw = CRustForeignVec::from_vec(Vec::<swig_subst_type!(T)>::new());
            CForeignVec!() { data: raw.data.cast(), len: raw.len, capacity: raw.capacity }
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn CForeignVecFree!()(v: CForeignVec!()) {
            drop_foreign_class_vec::<swig_subst_type!(T)>(CRustForeignVec {
                data: v.data.cast(), len: v.len, capacity: v.capacity,
            });
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn CForeignVecPush!()(v: *mut CForeignVec!(), e: *mut ::std::os::raw::c_void) {
            let v = unsafe { &mut *v };
            let mut raw = CRustForeignVec { data: v.data.cast(), len: v.len, capacity: v.capacity };
            push_foreign_class_to_vec::<swig_subst_type!(T)>(&mut raw, e);
            v.data = raw.data.cast();
            v.len = raw.len;
            v.capacity = raw.capacity;
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn CForeignVecRemove!()(v: *mut CForeignVec!(), idx: usize) -> *mut ::std::os::raw::c_void {
            let v = unsafe { &mut *v };
            let mut raw = CRustForeignVec { data: v.data.cast(), len: v.len, capacity: v.capacity };
            let elem = remove_foreign_class_from_vec::<swig_subst_type!(T)>(&mut raw, idx);
            v.data = raw.data.cast();
            v.len = raw.len;
            v.capacity = raw.capacity;
            elem
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn CForeignVecCloneAt!()(v: CForeignVec!(), idx: usize) -> *mut ::std::os::raw::c_void {
            let slice: &[swig_subst_type!(T)] = unsafe {
                (CRustSlice { data: v.data.cast(), len: v.len }).as_slice()
            };
            SwigForeignClass::box_object(slice[idx].clone())
        }
    );

    foreign_code!(module = "CForeignVecModule!().h";
                    r##"
#ifdef __cplusplus
#include "rust_vec_impl.hpp"
#include "CIndirectSlice!().h"

namespace $RUST_SWIG_USER_NAMESPACE {
using CForeignVecModule!() = RustVec<CForeignVec!(), internal::IndirectForeignVecPolicy<swig_f_type!(&[T], output), CIndirectSliceAccess!(), CForeignVec!(), CForeignVecNew!(), CForeignVecFree!(), CForeignVecPush!(), CForeignVecRemove!(), CForeignVecCloneAt!()>>;
}
#endif
"##);

    ($p:r_type) <T: SwigForeignClassIndirectAccess> Vec<T> => CForeignVec!() {
        let raw = CRustForeignVec::from_vec($p);
        $out = CForeignVec!() { data: raw.data.cast(), len: raw.len, capacity: raw.capacity };
    };
    ($p:r_type) <T: SwigForeignClassIndirectAccess> Vec<T> <= CForeignVec!() {
        $out = unsafe { Vec::from_raw_parts($p.data.cast(), $p.len, $p.capacity) };
    };
    ($p:f_type, req_modules = ["\"CForeignVecModule!().h\""]) => "CForeignVecModule!()"
        "CForeignVecModule!(){$p}";
    ($p:f_type, req_modules = ["\"CForeignVecModule!().h\""]) <= "CForeignVecModule!()"
        "$p.release()";
);

// A class's constructor return type can contain lifetimes. Keep the C
// descriptor's element type opaque so the descriptor itself stays lifetime-free.
foreign_typemap!(
    generic_alias!(CForeignVecModule = swig_concat_idents!(RustForeignVec, swig_f_type!(T)));
    generic_alias!(CForeignVec = swig_concat_idents!(CRustForeignVec, swig_f_type!(T)));
    generic_alias!(CForeignVecElem = swig_concat_idents!(CRustForeignVec, swig_f_type!(T), Elem));
    generic_alias!(CForeignVecNew = swig_concat_idents!(RustForeignVec, swig_f_type!(T), _new));
    generic_alias!(CForeignVecFree = swig_concat_idents!(RustForeignVec, swig_f_type!(T), _free));
    generic_alias!(CForeignVecPush = swig_concat_idents!(RustForeignVec, swig_f_type!(T), _push));
    generic_alias!(CForeignVecRemove = swig_concat_idents!(RustForeignVec, swig_f_type!(T), _remove));

    define_c_type!(
        module = "CForeignVecModule!().h";
        #[repr(C)]
        pub struct CForeignVecElem!() { _unused: u8 }

        #[repr(C)]
        #[derive(Clone, Copy)]
        pub struct CForeignVec!() {
            data: *mut CForeignVecElem!(),
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

#include "swig_f_type!(T)_fwd.hpp"
#include "rust_vec_impl.hpp"

namespace $RUST_SWIG_USER_NAMESPACE {
using CForeignVecModule!() = RustVec<CForeignVec!(), internal::ForeignVecPolicy<swig_f_type!(&T, output), CForeignVec!(), CForeignVecNew!(), CForeignVecFree!(), CForeignVecPush!(), CForeignVecRemove!()>>;
}
#endif
"##);

    ($p:r_type) <T: SwigForeignClassDirectVecAccess> Vec<T> => CForeignVec!() {
        let mut v: Vec<swig_subst_type!(T)> = $p;
        $out = CForeignVec!() {
            data: v.as_mut_ptr().cast(), len: v.len(), capacity: v.capacity(),
        };
        ::std::mem::forget(v);
    };
    ($p:r_type) <T: SwigForeignClassDirectVecAccess> Vec<T> <= CForeignVec!() {
        $out = unsafe { Vec::from_raw_parts($p.data.cast(), $p.len, $p.capacity) };
    };
    ($p:f_type, req_modules = ["\"CForeignVecModule!().h\""]) => "CForeignVecModule!()"
        "CForeignVecModule!(){$p}";
    ($p:f_type, req_modules = ["\"CForeignVecModule!().h\""]) <= "CForeignVecModule!()"
        "$p.release()";
);

// The outer allocation contains Rust Vec<T> values. C++ never indexes this
// allocation directly: it gets a borrowed slice for each row from Rust.
foreign_typemap!(
    generic_alias!(COuterVecModule = swig_concat_idents!(RustVecVec, swig_f_type!(T)));
    generic_alias!(COuterVec = swig_concat_idents!(CRustVecVec, swig_f_type!(T)));
    generic_alias!(COuterVecElem = swig_concat_idents!(CRustVecVec, swig_f_type!(T), Elem));
    generic_alias!(COuterRowSlice = swig_concat_idents!(CRustVecVec, swig_f_type!(T), RowSlice));
    generic_alias!(COuterRowElem = swig_concat_idents!(CRustVecVec, swig_f_type!(T), RowElem));
    generic_alias!(COuterVecNew = swig_concat_idents!(RustVecVec, swig_f_type!(T), _new));
    generic_alias!(COuterVecFree = swig_concat_idents!(RustVecVec, swig_f_type!(T), _free));
    generic_alias!(COuterVecGet = swig_concat_idents!(RustVecVec, swig_f_type!(T), _get));
    generic_alias!(COuterVecPush = swig_concat_idents!(RustVecVec, swig_f_type!(T), _push));
    generic_alias!(COuterVecRemove = swig_concat_idents!(RustVecVec, swig_f_type!(T), _remove));
    generic_alias!(CInnerVec = swig_concat_idents!(CRustForeignVec, swig_f_type!(T)));
    generic_alias!(CInnerVecModule = swig_concat_idents!(RustForeignVec, swig_f_type!(T)));
    // Resolve the inner vector typemap before define_c_type! uses its C struct.
    generic_alias!(InnerVecDependency = swig_f_type!(Vec<T>));

    define_c_type!(
        module = "COuterVecModule!().h";
        #[repr(C)]
        pub struct COuterVecElem!() { _unused: u8 }

        #[repr(C)]
        pub struct COuterRowElem!() { _unused: u8 }

        #[repr(C)]
        #[derive(Clone, Copy)]
        pub struct COuterRowSlice!() {
            data: *const COuterRowElem!(),
            len: usize,
        }

        #[repr(C)]
        #[derive(Clone, Copy)]
        pub struct COuterVec!() {
            data: *mut COuterVecElem!(),
            len: usize,
            capacity: usize,
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn COuterVecNew!()() -> COuterVec!() {
            let raw = CRustForeignVec::from_vec(Vec::<Vec<swig_subst_type!(T)>>::new());
            COuterVec!() { data: raw.data.cast(), len: raw.len, capacity: raw.capacity }
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn COuterVecFree!()(v: COuterVec!()) {
            let rows: Vec<Vec<swig_subst_type!(T)>> = unsafe {
                Vec::from_raw_parts(v.data.cast(), v.len, v.capacity)
            };
            drop(rows);
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn COuterVecGet!()(v: COuterVec!(), idx: usize) -> COuterRowSlice!() {
            let rows: &[Vec<swig_subst_type!(T)>] = unsafe {
                ::std::slice::from_raw_parts(v.data.cast(), v.len)
            };
            let row = &rows[idx];
            COuterRowSlice!() { data: row.as_ptr().cast(), len: row.len() }
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn COuterVecPush!()(v: *mut COuterVec!(), row: CInnerVec!()) {
            let v = unsafe { &mut *v };
            let mut rows: Vec<Vec<swig_subst_type!(T)>> = unsafe {
                Vec::from_raw_parts(v.data.cast(), v.len, v.capacity)
            };
            let row: Vec<swig_subst_type!(T)> = unsafe {
                Vec::from_raw_parts(row.data.cast(), row.len, row.capacity)
            };
            rows.push(row);
            let raw = CRustForeignVec::from_vec(rows);
            v.data = raw.data.cast();
            v.len = raw.len;
            v.capacity = raw.capacity;
        }

        #[unsafe(no_mangle)]
        pub extern "C" fn COuterVecRemove!()(v: *mut COuterVec!(), idx: usize) -> CInnerVec!() {
            let v = unsafe { &mut *v };
            let mut rows: Vec<Vec<swig_subst_type!(T)>> = unsafe {
                Vec::from_raw_parts(v.data.cast(), v.len, v.capacity)
            };
            let row = rows.remove(idx);
            let raw = CRustForeignVec::from_vec(rows);
            v.data = raw.data.cast();
            v.len = raw.len;
            v.capacity = raw.capacity;
            let row = CRustForeignVec::from_vec(row);
            CInnerVec!() { data: row.data.cast(), len: row.len, capacity: row.capacity }
        }
    );

    foreign_code!(module = "COuterVecModule!().h";
                    r##"
#ifdef __cplusplus
#include "rust_vec_impl.hpp"
#include "CInnerVecModule!().h"

namespace $RUST_SWIG_USER_NAMESPACE {
using COuterVecModule!() = RustVec<COuterVec!(), internal::NestedForeignVecPolicy<
    swig_f_type!(Vec<T>, output), RustSlice<const swig_f_type!(T, output)>,
    COuterVec!(), CInnerVec!(), COuterRowSlice!(),
    COuterVecNew!(), COuterVecFree!(), COuterVecGet!(), COuterVecPush!(), COuterVecRemove!()>>;
}
#endif
"##);

    ($p:r_type) <T: SwigForeignClassDirectVecAccess> Vec<Vec<T>> => COuterVec!() {
        let raw = CRustForeignVec::from_vec($p);
        $out = COuterVec!() { data: raw.data.cast(), len: raw.len, capacity: raw.capacity };
    };
    ($p:r_type) <T: SwigForeignClassDirectVecAccess> Vec<Vec<T>> <= COuterVec!() {
        $out = unsafe { Vec::from_raw_parts($p.data.cast(), $p.len, $p.capacity) };
    };
    ($p:f_type, req_modules = ["\"COuterVecModule!().h\""]) => "COuterVecModule!()"
        "COuterVecModule!(){$p}";
    ($p:f_type, req_modules = ["\"COuterVecModule!().h\""]) <= "COuterVecModule!()"
        "$p.release()";
);
