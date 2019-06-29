mod swig_foreign_types_map {
    #![swig_foreigner_type = "void"]
    #![swig_rust_type = "()"]
    #![swig_foreigner_type = "int8_t"]
    #![swig_rust_type = "i8"]
    #![swig_foreigner_type = "uint8_t"]
    #![swig_rust_type = "u8"]
    #![swig_foreigner_type = "int16_t"]
    #![swig_rust_type = "i16"]
    #![swig_foreigner_type = "uint16_t"]
    #![swig_rust_type = "u16"]
    #![swig_foreigner_type = "int32_t"]
    #![swig_rust_type = "i32"]
    #![swig_foreigner_type = "uint32_t"]
    #![swig_rust_type = "u32"]
    #![swig_foreigner_type = "int64_t"]
    #![swig_rust_type = "i64"]
    #![swig_foreigner_type = "uint64_t"]
    #![swig_rust_type = "u64"]
    #![swig_foreigner_type = "uintptr_t"]
    #![swig_rust_type = "usize"]
    #![swig_foreigner_type = "float"]
    #![swig_rust_type = "f32"]
    #![swig_foreigner_type = "double"]
    #![swig_rust_type = "f64"]
    #![swig_foreigner_type = "char"]
    #![swig_rust_type = "::std::os::raw::c_char"]
    #![swig_foreigner_type = "const char *"]
    #![swig_rust_type = "*const ::std::os::raw::c_char"]
    #![swig_foreigner_type = "int"]
    #![swig_rust_type = "::std::os::raw::c_int"]
    #![swig_foreigner_type = "struct CRustVecU8"]
    #![swig_rust_type = "CRustVecU8"]
    #![swig_foreigner_type = "struct CRustVecI32"]
    #![swig_rust_type = "CRustVecI32"]
    #![swig_foreigner_type = "struct CRustVecU32"]
    #![swig_rust_type = "CRustVecU32"]
    #![swig_foreigner_type = "struct CRustVecUsize"]
    #![swig_rust_type = "CRustVecUsize"]
    #![swig_foreigner_type = "struct CRustVecF32"]
    #![swig_rust_type = "CRustVecF32"]
    #![swig_foreigner_type = "struct CRustVecF64"]
    #![swig_rust_type = "CRustVecF64"]
    #![swig_foreigner_type = "struct CRustForeignVec"]
    #![swig_rust_type = "CRustForeignVec"]
    #![swig_foreigner_type = "struct CResultObjectString"]
    #![swig_rust_type = "CResultObjectString"]
    #![swig_foreigner_type = "struct CResultCRustForeignVecString"]
    #![swig_rust_type = "CResultCRustForeignVecString"]
    #![swig_foreigner_type = "struct CResultObjectEnum"]
    #![swig_rust_type = "CResultObjectEnum"]
    #![swig_foreigner_type = "struct CResultObjectObject"]
    #![swig_rust_type = "CResultObjectObject"]
    #![swig_foreigner_type = "struct CResultVecObjectObject"]
    #![swig_rust_type = "CResultVecObjectObject"]
    #![swig_foreigner_type = "struct CResultCRustVecU8Object"]
    #![swig_rust_type = "CResultCRustVecU8Object"]
    #![swig_foreigner_type = "struct CResultI64Object"]
    #![swig_rust_type = "CResultI64Object"]
}

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
#[swig_code = "let mut {to_var}: {to_var_type} = {from_var}.swig_into();"]
trait SwigInto<T> {
    fn swig_into(self) -> T;
}

#[allow(dead_code)]
#[swig_code = "let mut {to_var}: {to_var_type} = <{to_var_type}>::swig_from({from_var});"]
trait SwigFrom<T> {
    fn swig_from(_: T) -> Self;
}

#[allow(dead_code)]
#[swig_code = "let mut {to_var}: {to_var_type} = {from_var}.swig_deref();"]
trait SwigDeref {
    type Target: ?Sized;
    fn swig_deref(&self) -> &Self::Target;
}

#[allow(dead_code)]
#[swig_code = "let mut {to_var}: {to_var_type} = {from_var}.swig_deref_mut();"]
trait SwigDerefMut {
    type Target: ?Sized;
    fn swig_deref_mut(&mut self) -> &mut Self::Target;
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

impl<T> SwigDeref for Arc<Mutex<T>> {
    type Target = Mutex<T>;
    fn swig_deref(&self) -> &Mutex<T> {
        self
    }
}

impl<'a, T> SwigFrom<&'a Mutex<T>> for MutexGuard<'a, T> {
    fn swig_from(m: &'a Mutex<T>) -> MutexGuard<'a, T> {
        m.lock().unwrap()
    }
}

impl<'a, T> SwigDeref for MutexGuard<'a, T> {
    type Target = T;
    fn swig_deref(&self) -> &T {
        self
    }
}

impl<'a, T> SwigDerefMut for MutexGuard<'a, T> {
    type Target = T;
    fn swig_deref_mut(&mut self) -> &mut T {
        self
    }
}

impl<T> SwigDeref for Rc<T> {
    type Target = T;
    fn swig_deref(&self) -> &T {
        self
    }
}

impl<'a, T> SwigDeref for &'a Rc<T> {
    type Target = T;
    fn swig_deref(&self) -> &T {
        self
    }
}

impl<'a, T> SwigFrom<&'a RefCell<T>> for Ref<'a, T> {
    fn swig_from(m: &'a RefCell<T>) -> Ref<'a, T> {
        m.borrow()
    }
}

impl<'a, T> SwigFrom<&'a RefCell<T>> for RefMut<'a, T> {
    fn swig_from(m: &'a RefCell<T>) -> RefMut<'a, T> {
        m.borrow_mut()
    }
}

impl<'a, T> SwigDeref for Ref<'a, T> {
    type Target = T;
    fn swig_deref(&self) -> &T {
        self
    }
}

impl<'a, T> SwigDerefMut for RefMut<'a, T> {
    type Target = T;
    fn swig_deref_mut(&mut self) -> &mut T {
        self
    }
}

impl<T: SwigForeignClass> SwigDeref for T {
    type Target = T;
    fn swig_deref(&self) -> &T {
        self
    }
}

impl<T: SwigForeignClass> SwigDerefMut for T {
    type Target = T;
    fn swig_deref_mut(&mut self) -> &mut T {
        self
    }
}

#[allow(dead_code)]
#[repr(C)]
#[derive(Copy, Clone)]
pub struct CRustVecU8 {
    data: *const u8,
    len: usize,
    capacity: usize,
}

#[allow(dead_code)]
impl CRustVecU8 {
    pub fn from_vec(mut v: Vec<u8>) -> CRustVecU8 {
        let p = v.as_mut_ptr();
        let len = v.len();
        let cap = v.capacity();
        ::std::mem::forget(v);
        CRustVecU8 {
            data: p,
            len: len,
            capacity: cap,
        }
    }
}

impl SwigFrom<Vec<u8>> for CRustVecU8 {
    fn swig_from(v: Vec<u8>) -> CRustVecU8 {
        CRustVecU8::from_vec(v)
    }
}

#[allow(private_no_mangle_fns)]
#[no_mangle]
pub extern "C" fn CRustVecU8_free(v: CRustVecU8) {
    let v = unsafe { Vec::from_raw_parts(v.data as *mut u8, v.len, v.capacity) };
    drop(v);
}

#[allow(dead_code)]
#[repr(C)]
pub struct CRustVecI32 {
    data: *const i32,
    len: usize,
    capacity: usize,
}

impl SwigFrom<Vec<i32>> for CRustVecI32 {
    fn swig_from(mut v: Vec<i32>) -> CRustVecI32 {
        let p = v.as_mut_ptr();
        let len = v.len();
        let cap = v.capacity();
        ::std::mem::forget(v);
        CRustVecI32 {
            data: p,
            len: len,
            capacity: cap,
        }
    }
}

#[allow(private_no_mangle_fns)]
#[no_mangle]
pub extern "C" fn CRustVecI32_free(v: CRustVecI32) {
    let v = unsafe { Vec::from_raw_parts(v.data as *mut i32, v.len, v.capacity) };
    drop(v);
}

#[allow(dead_code)]
#[repr(C)]
pub struct CRustVecU32 {
    data: *const u32,
    len: usize,
    capacity: usize,
}

impl SwigFrom<Vec<u32>> for CRustVecU32 {
    fn swig_from(mut v: Vec<u32>) -> CRustVecU32 {
        let p = v.as_mut_ptr();
        let len = v.len();
        let cap = v.capacity();
        ::std::mem::forget(v);
        CRustVecU32 {
            data: p,
            len: len,
            capacity: cap,
        }
    }
}

#[allow(private_no_mangle_fns)]
#[no_mangle]
pub extern "C" fn CRustVecU32_free(v: CRustVecU32) {
    let v = unsafe { Vec::from_raw_parts(v.data as *mut u32, v.len, v.capacity) };
    drop(v);
}

#[allow(dead_code)]
#[repr(C)]
pub struct CRustVecUsize {
    data: *const usize,
    len: usize,
    capacity: usize,
}

impl SwigFrom<Vec<usize>> for CRustVecUsize {
    fn swig_from(mut v: Vec<usize>) -> CRustVecUsize {
        let p = v.as_mut_ptr();
        let len = v.len();
        let cap = v.capacity();
        ::std::mem::forget(v);
        CRustVecUsize {
            data: p,
            len: len,
            capacity: cap,
        }
    }
}

#[allow(private_no_mangle_fns)]
#[no_mangle]
pub extern "C" fn CRustVecUsize_free(v: CRustVecUsize) {
    let v = unsafe { Vec::from_raw_parts(v.data as *mut usize, v.len, v.capacity) };
    drop(v);
}

#[allow(dead_code)]
#[repr(C)]
pub struct CRustVecF32 {
    data: *const f32,
    len: usize,
    capacity: usize,
}

impl SwigFrom<Vec<f32>> for CRustVecF32 {
    fn swig_from(mut v: Vec<f32>) -> CRustVecF32 {
        let p = v.as_mut_ptr();
        let len = v.len();
        let cap = v.capacity();
        ::std::mem::forget(v);
        CRustVecF32 {
            data: p,
            len: len,
            capacity: cap,
        }
    }
}

#[allow(private_no_mangle_fns)]
#[no_mangle]
pub extern "C" fn CRustVecF32_free(v: CRustVecF32) {
    let v = unsafe { Vec::from_raw_parts(v.data as *mut f32, v.len, v.capacity) };
    drop(v);
}

#[allow(dead_code)]
#[repr(C)]
pub struct CRustVecF64 {
    data: *const f64,
    len: usize,
    capacity: usize,
}

impl SwigFrom<Vec<f64>> for CRustVecF64 {
    fn swig_from(mut v: Vec<f64>) -> CRustVecF64 {
        let p = v.as_mut_ptr();
        let len = v.len();
        let cap = v.capacity();
        ::std::mem::forget(v);
        CRustVecF64 {
            data: p,
            len: len,
            capacity: cap,
        }
    }
}

#[allow(private_no_mangle_fns)]
#[no_mangle]
pub extern "C" fn CRustVecF64_free(v: CRustVecF64) {
    let v = unsafe { Vec::from_raw_parts(v.data as *mut f64, v.len, v.capacity) };
    drop(v);
}

#[allow(dead_code)]
#[repr(C)]
#[derive(Copy, Clone)]
pub struct CRustForeignVec {
    data: *const ::std::os::raw::c_void,
    len: usize,
    capacity: usize,
    step: usize,
}

#[allow(dead_code)]
impl CRustForeignVec {
    pub fn from_vec<T: SwigForeignClass>(mut v: Vec<T>) -> CRustForeignVec {
        let data = v.as_mut_ptr() as *const ::std::os::raw::c_void;
        let len = v.len();
        let capacity = v.capacity();
        ::std::mem::forget(v);
        CRustForeignVec {
            data,
            len,
            capacity,
            step: ::std::mem::size_of::<T>(),
        }
    }
}

impl<T: SwigForeignClass> SwigFrom<Vec<T>> for CRustForeignVec {
    fn swig_from(v: Vec<T>) -> Self {
        CRustForeignVec::from_vec(v)
    }
}

impl<T: SwigForeignClass> SwigInto<Vec<T>> for CRustForeignVec {
    fn swig_into(self) -> Vec<T> {
        unsafe { Vec::from_raw_parts(self.data as *mut T, self.len, self.capacity) }
    }
}

#[allow(dead_code)]
fn drop_foreign_class_vec<T: SwigForeignClass>(data: *mut T, len: usize, cap: usize) {
    let v = unsafe { Vec::from_raw_parts(data, len, cap) };
    drop(v);
}

#[allow(dead_code)]
#[inline]
fn push_foreign_class_to_vec<T: SwigForeignClass>(
    vec: *mut CRustForeignVec,
    elem: *mut ::std::os::raw::c_void,
) {
    assert!(!vec.is_null());
    let vec: &mut CRustForeignVec = unsafe { &mut *vec };
    assert!(vec.len == 0 || ::std::mem::size_of::<T>() == vec.step);
    vec.step = ::std::mem::size_of::<T>();
    let mut v = unsafe { Vec::from_raw_parts(vec.data as *mut T, vec.len, vec.capacity) };
    v.push(T::unbox_object(elem));
    vec.data = v.as_mut_ptr() as *const ::std::os::raw::c_void;
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
    assert_eq!(::std::mem::size_of::<T>(), vec.step);
    let mut v = unsafe { Vec::from_raw_parts(vec.data as *mut T, vec.len, vec.capacity) };
    let elem: T = v.remove(index);
    vec.data = v.as_mut_ptr() as *const ::std::os::raw::c_void;
    vec.len = v.len();
    vec.capacity = v.capacity();
    ::std::mem::forget(v);
    T::box_object(elem)
}

// &str -> &Path
impl<'a> SwigInto<&'a Path> for &'a str {
    fn swig_into(self) -> &'a Path {
        Path::new(self)
    }
}

#[allow(dead_code)]
#[repr(C)]
#[derive(Copy, Clone)]
pub struct CRustString {
    data: *const ::std::os::raw::c_char,
    len: usize,
    capacity: usize,
}

#[allow(dead_code)]
impl CRustString {
    pub fn from_string(s: String) -> CRustString {
        let data = s.as_ptr() as *const ::std::os::raw::c_char;
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

#[allow(dead_code)]
#[repr(C)]
pub struct CResultObjectString {
    is_ok: u8,
    data: CResultObjectStringUnion,
}

#[allow(dead_code)]
#[repr(C)]
#[derive(Copy, Clone)]
pub union CResultObjectStringUnion {
    pub ok: *mut ::std::os::raw::c_void,
    pub err: CRustString,
}

impl SwigFrom<Result<(), String>> for CResultObjectString {
    fn swig_from(x: Result<(), String>) -> Self {
        match x {
            Ok(_) => CResultObjectString {
                is_ok: 1,
                data: CResultObjectStringUnion {
                    ok: ::std::ptr::null_mut(),
                },
            },
            Err(err) => CResultObjectString {
                is_ok: 0,
                data: CResultObjectStringUnion {
                    err: CRustString::from_string(err),
                },
            },
        }
    }
}

impl<T: SwigForeignClass> SwigFrom<Result<T, String>> for CResultObjectString {
    fn swig_from(x: Result<T, String>) -> Self {
        match x {
            Ok(v) => CResultObjectString {
                is_ok: 1,
                data: CResultObjectStringUnion {
                    ok: <T>::box_object(v),
                },
            },
            Err(err) => CResultObjectString {
                is_ok: 0,
                data: CResultObjectStringUnion {
                    err: CRustString::from_string(err),
                },
            },
        }
    }
}

#[allow(dead_code)]
#[repr(C)]
pub struct CResultCRustForeignVecString {
    is_ok: u8,
    data: CResultCRustForeignVecStringUnion,
}

#[allow(dead_code)]
#[repr(C)]
#[derive(Copy, Clone)]
pub union CResultCRustForeignVecStringUnion {
    pub ok: CRustForeignVec,
    pub err: CRustString,
}

impl<T: SwigForeignClass> SwigFrom<Result<Vec<T>, String>> for CResultCRustForeignVecString {
    fn swig_from(x: Result<Vec<T>, String>) -> Self {
        match x {
            Ok(v) => CResultCRustForeignVecString {
                is_ok: 1,
                data: CResultCRustForeignVecStringUnion {
                    ok: CRustForeignVec::from_vec(v),
                },
            },
            Err(err) => CResultCRustForeignVecString {
                is_ok: 0,
                data: CResultCRustForeignVecStringUnion {
                    err: CRustString::from_string(err),
                },
            },
        }
    }
}

#[allow(dead_code)]
#[repr(C)]
pub struct CResultObjectObject {
    is_ok: u8,
    data: CResultObjectObjectUnion,
}

#[allow(dead_code)]
#[repr(C)]
#[derive(Copy, Clone)]
pub union CResultObjectObjectUnion {
    pub ok: *mut ::std::os::raw::c_void,
    pub err: *mut ::std::os::raw::c_void,
}

impl<ErrT: SwigForeignClass> SwigFrom<Result<(), ErrT>> for CResultObjectObject {
    fn swig_from(x: Result<(), ErrT>) -> Self {
        match x {
            Ok(_) => CResultObjectObject {
                is_ok: 1,
                data: CResultObjectObjectUnion {
                    ok: ::std::ptr::null_mut(),
                },
            },
            Err(err) => CResultObjectObject {
                is_ok: 0,
                data: CResultObjectObjectUnion {
                    err: <ErrT>::box_object(err),
                },
            },
        }
    }
}

impl<T: SwigForeignClass, ErrT: SwigForeignClass> SwigFrom<Result<T, ErrT>>
    for CResultObjectObject
{
    fn swig_from(x: Result<T, ErrT>) -> Self {
        match x {
            Ok(v) => CResultObjectObject {
                is_ok: 1,
                data: CResultObjectObjectUnion {
                    ok: <T>::box_object(v),
                },
            },
            Err(err) => CResultObjectObject {
                is_ok: 0,
                data: CResultObjectObjectUnion {
                    err: <ErrT>::box_object(err),
                },
            },
        }
    }
}

#[allow(dead_code)]
#[repr(C)]
pub struct CResultVecObjectObject {
    is_ok: u8,
    data: CResultVecObjectObjectUnion,
}

#[allow(dead_code)]
#[repr(C)]
#[derive(Copy, Clone)]
pub union CResultVecObjectObjectUnion {
    pub ok: CRustForeignVec,
    pub err: *mut ::std::os::raw::c_void,
}

impl<T: SwigForeignClass, ErrT: SwigForeignClass> SwigFrom<Result<Vec<T>, ErrT>>
    for CResultVecObjectObject
{
    fn swig_from(x: Result<Vec<T>, ErrT>) -> Self {
        match x {
            Ok(v) => CResultVecObjectObject {
                is_ok: 1,
                data: CResultVecObjectObjectUnion {
                    ok: CRustForeignVec::from_vec(v),
                },
            },
            Err(err) => CResultVecObjectObject {
                is_ok: 0,
                data: CResultVecObjectObjectUnion {
                    err: <ErrT>::box_object(err),
                },
            },
        }
    }
}

#[allow(dead_code)]
#[repr(C)]
pub struct CResultCRustVecU8Object {
    data: CRustVecU8ObjectUnion,
    is_ok: u8,
}

#[allow(dead_code)]
#[repr(C)]
#[derive(Copy, Clone)]
pub union CRustVecU8ObjectUnion {
    pub ok: CRustVecU8,
    pub err: *mut ::std::os::raw::c_void,
}

impl<ErrT: SwigForeignClass> SwigFrom<Result<Vec<u8>, ErrT>> for CResultCRustVecU8Object {
    fn swig_from(x: Result<Vec<u8>, ErrT>) -> Self {
        match x {
            Ok(v) => CResultCRustVecU8Object {
                is_ok: 1,
                data: CRustVecU8ObjectUnion {
                    ok: CRustVecU8::from_vec(v),
                },
            },
            Err(err) => CResultCRustVecU8Object {
                is_ok: 0,
                data: CRustVecU8ObjectUnion {
                    err: <ErrT>::box_object(err),
                },
            },
        }
    }
}

#[allow(dead_code)]
#[repr(C)]
pub struct CResultObjectEnum {
    data: CResultObjectEnumUnion,
    is_ok: u8,
}

#[allow(dead_code)]
#[repr(C)]
#[derive(Copy, Clone)]
pub union CResultObjectEnumUnion {
    pub ok: *mut ::std::os::raw::c_void,
    pub err: u32,
}

impl<T, ErrT> SwigFrom<Result<T, ErrT>> for CResultObjectEnum
where
    T: SwigForeignClass,
    ErrT: SwigForeignEnum,
{
    fn swig_from(x: Result<T, ErrT>) -> Self {
        match x {
            Ok(x) => CResultObjectEnum {
                data: CResultObjectEnumUnion {
                    ok: <T>::box_object(x),
                },
                is_ok: 1,
            },
            Err(e) => CResultObjectEnum {
                data: CResultObjectEnumUnion { err: e.as_u32() },
                is_ok: 0,
            },
        }
    }
}

#[allow(dead_code)]
#[repr(C)]
pub struct CResultI64Object {
    data: CResultI64ObjectUnion,
    is_ok: u8,
}

#[allow(dead_code)]
#[repr(C)]
#[derive(Copy, Clone)]
pub union CResultI64ObjectUnion {
    pub ok: i64,
    pub err: *mut ::std::os::raw::c_void,
}

impl<ErrT> SwigFrom<Result<i64, ErrT>> for CResultI64Object
where
    ErrT: SwigForeignClass,
{
    fn swig_from(x: Result<i64, ErrT>) -> Self {
        match x {
            Ok(x) => CResultI64Object {
                data: CResultI64ObjectUnion { ok: x },
                is_ok: 1,
            },
            Err(e) => CResultI64Object {
                data: CResultI64ObjectUnion {
                    err: <ErrT>::box_object(e),
                },
                is_ok: 0,
            },
        }
    }
}

impl<'a> SwigInto<String> for &'a str {
    fn swig_into(self) -> String {
        self.into()
    }
}

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
    (f_type) "/*const*/void *";
);

foreign_typemap!(
    (r_type) <T: SwigTypeIsReprC> *const T;
    (f_type) "const swig_f_type!(T) *";
);

foreign_typemap!(
     generic_alias!(CRustPair = swig_concat_idents!(CRustPair, swig_i_type!(T1), swig_i_type!(T2)));
     define_c_type!(
         module = "rust_tuple.h";
         #[repr(C)]
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
        }
    };
    ($p:r_type) <T1, T2> (T1, T2) <= CRustPair!() {
        swig_from_i_type_to_rust!(T1, $p.first, p0)
        swig_from_i_type_to_rust!(T2, $p.second, p1)
        $out = (p0, p1)
    };
    ($p:f_type, req_modules = ["\"rust_tuple.h\"", "<utility>"]) => "std::pair<swig_f_type!(T1), swig_f_type!(T2)>"
            "std::make_pair(swig_foreign_from_i_type!(T1, $p.first), swig_foreign_from_i_type!(T2, $p.second))";
    ($p:f_type, req_modules = ["\"rust_tuple.h\"", "<utility>"]) <= "std::pair<swig_f_type!(T1), swig_f_type!(T2)>"
        "swig_f_type!(CRustPair!()) { swig_foreign_to_i_type!(T1, $p.first), swig_foreign_to_i_type!(T2, $p.second) }";
 );

foreign_typemap!(
    ($pin:r_type) bool => ::std::os::raw::c_char {
        $out = if $pin  { 1 } else { 0 }
    };
    ($pin:f_type) => "bool" "($pin != 0)";
    ($pin:r_type) bool <= ::std::os::raw::c_char {
        $out = $pin != 0
    };
    ($pin:f_type) <= "bool" "$pin ? 1 : 0";
);

foreign_typemap!(
    define_c_type!(module = "rust_str.h";
                   #[repr(C)]
                   pub struct CRustStrView {
                       data: *const ::std::os::raw::c_char,
                       len: usize,
                   }
    );
    ($p:r_type) &str => CRustStrView {
        $out = CRustStrView::from_str($p)
    };
    ($p:r_type) &str <= CRustStrView {
        $out = unsafe {
            let slice: &[u8] = ::std::slice::from_raw_parts($p.data as *const u8, $p.len);
            ::std::str::from_utf8_unchecked(slice)
        }
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
    define_c_type!(module = "rust_str.h";
        #[repr(C)]
        struct CRustString {
            data: *const ::std::os::raw::c_char,
            len: usize,
            capacity: usize,
        }

        #[no_mangle]
        pub extern "C" fn crust_string_free(x: CRustString) {
            let s = unsafe { String::from_raw_parts(x.data as *mut u8, x.len, x.capacity) };
            drop(s);
        }

        #[no_mangle]
        pub extern "C" fn crust_string_clone(x: CRustString) -> CRustString {
            let s = unsafe { String::from_raw_parts(x.data as *mut u8, x.len, x.capacity) };
            let ret = CRustString::from_string(s.clone());
            ::std::mem::forget(s);
            ret
        }
    );
    foreigner_code!(module = "rust_str.h";
                    r##"
#ifdef __cplusplus

#include <string>
"##
    );
    foreigner_code!(module = "rust_str.h";
                    option = "CppStrView::Std17";
                    r##"
#include <string_view>
"##);
    foreigner_code!(module = "rust_str.h";
                    option = "CppStrView::Boost";
                    r##"
#include <boost/utility/string_view.hpp>
"##);
    foreigner_code!(module = "rust_str.h";
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
"##);
    foreigner_code!(module = "rust_str.h";
                    option = "CppStrView::Std17";
                    r##"
    std::string_view to_string_view() const { return std::string_view(data, len); }
"##);
    foreigner_code!(module = "rust_str.h";
                    option = "CppStrView::Boost";
                    r#"
    boost::string_view to_boost_string_view() const { return boost::string_view{ data, len }; }
"#);
    foreigner_code!(module = "rust_str.h";
                    r##"
private:
    void free_mem() noexcept
    {
        if (data != nullptr) {
            crust_string_free(*this);
            reset(*this);
        }
    }
    static void reset(RustString &o) noexcept
    {
        o.data = nullptr;
        o.len = 0;
        o.capacity = 0;
    }
};
} // namespace $RUST_SWIG_USER_NAMESPACE
#endif // __cplusplus
"##
                    );
    ($pin:r_type) String => CRustString {
        $out = CRustString::from_string($pin)
    };
    ($pin:f_type, req_modules = ["\"rust_str.h\""]) => "RustString" "RustString{$pin}";
);

foreign_typemap!(
    generic_alias!(CRustOpt = swig_concat_idents!(CRustOption, swig_i_type!(T)));
    generic_alias!(CRustOptUnion = swig_concat_idents!(CRustOptionUnion, swig_i_type!(T)));
     define_c_type!(
         module = "rust_option.h";
         #[repr(C)]
         pub union CRustOptUnion!() {
             data: swig_i_type!(T),
             uninit: u8,
         }

         #[repr(C)]
         pub struct CRustOpt!() {
             val: CRustOptUnion!(),
             is_some: u8,
         }
     );
    ($p:r_type) <T> Option<T> => CRustOpt!() {
        $out = match $p {
            Some(x) => {
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
        }
    };
    ($p:r_type) <T> Option<T> <= CRustOpt!() {
        $out = if $p.is_some != 0 {
            swig_from_i_type_to_rust!(T, unsafe { $p.val.data }, ret)
            Some(ret)
        } else {
            None
        }
    };

    ($p:f_type, option = "CppOptional::Boost", req_modules = ["\"rust_option.h\"", "<boost/optional.hpp>"]) => "boost::optional<swig_f_type!(T)>"
        "($p.is_some != 0) ? boost::optional<swig_f_type!(T)>(swig_foreign_from_i_type!(T, $p.val.data)) : boost::optional<swig_f_type!(T)>()";
    ($p:f_type, option = "CppOptional::Boost", req_modules = ["\"rust_option.h\"", "<boost/optional.hpp>"]) <= "boost::optional<swig_f_type!(T)>"
        r#"        $out;
        if (!!$p) {
            $out.val.data = swig_foreign_to_i_type!(T, (*$p));
            $out.is_some = 1;
        } else {
            $out.is_some = 0;
        }"#;

    ($p:f_type, option = "CppOptional::Std17", req_modules = ["\"rust_option.h\"", "<optional>"]) => "std::optional<swig_f_type!(T)>"
        "($p.is_some != 0) ? std::optional<swig_f_type!(T)>(swig_foreign_from_i_type!(T, $p.val.data)) : std::optional<swig_f_type!(T)>()";
    ($p:f_type, option = "CppOptional::Std17", req_modules = ["\"rust_option.h\"", "<optional>"]) <= "std::optional<swig_f_type!(T)>"
        r#"        $out;
        if (!!$p) {
            $out.val.data = swig_foreign_to_i_type!(T, (*$p));
            $out.is_some = 1;
        } else {
            $out.is_some = 0;
        }"#;
);

#[allow(dead_code)]
#[repr(C)]
pub struct CRustObjectSlice {
    data: *const ::std::os::raw::c_void,
    len: usize,
    step: usize,
}

foreign_typemap!(
    define_c_type!(
        module = "rust_slice.h";
        #[repr(C)]
        pub struct CRustObjectSlice {
            data: *const ::std::os::raw::c_void,
            len: usize,
            step: usize,
        });
    (r_type) CRustObjectSlice;
    (f_type) "CRustObjectSlice";
);

#[allow(dead_code)]
#[repr(C)]
pub struct CRustObjectMutSlice {
    data: *mut ::std::os::raw::c_void,
    len: usize,
    step: usize,
}

foreign_typemap!(
    define_c_type!(
        module = "rust_slice_mut.h";
        #[repr(C)]
        pub struct CRustObjectMutSlice {
            data: *mut ::std::os::raw::c_void,
            len: usize,
            step: usize,
        });
    (r_type) CRustObjectMutSlice;
    (f_type) "CRustObjectMutSlice";
);

foreign_typemap!(
     foreigner_code!(module = "rust_slice.h";
                    r##"
#include "rust_foreign_slice_impl.hpp"
"##);
    ($p:r_type) <T: SwigForeignClass> &[T] => CRustObjectSlice {
        $out = CRustObjectSlice {
            data: $p.as_ptr() as *const ::std::os::raw::c_void,
            len: $p.len(),
            step: ::std::mem::size_of::<swig_subst_type!(T)>(),
        }
    };
    ($p:r_type) <T: SwigForeignClass> &[T] <= CRustObjectSlice {
        $out = unsafe { ::std::slice::from_raw_parts($p.data as *const swig_subst_type!(T), $p.len) }
    };
    ($p:f_type, req_modules = ["\"rust_slice.h\""]) => "RustForeignSlice<swig_f_type!(&T), CRustObjectSlice>"
        "RustForeignSlice<swig_f_type!(&T), CRustObjectSlice>{$p}";
    ($p:f_type, req_modules = ["\"rust_slice.h\""]) <= "RustForeignSlice<swig_f_type!(&T, output), CRustObjectSlice>"
        "$p";
);

foreign_typemap!(
     foreigner_code!(module = "rust_slice_mut.h";
                    r##"
#ifdef __cplusplus
#include "rust_foreign_slice_impl.hpp"
#endif
"##);
    ($p:r_type) <T: SwigForeignClass> &mut [T] => CRustObjectMutSlice {
        $out = CRustObjectMutSlice {
            data: $p.as_ptr() as *const ::std::os::raw::c_void,
            len: $p.len(),
            step: ::std::mem::size_of::<swig_subst_type!(T)>(),
        }
    };
    ($p:r_type) <T: SwigForeignClass> &mut [T] <= CRustObjectMutSlice {
        $out = unsafe { ::std::slice::from_raw_parts_mut($p.data as *mut swig_subst_type!(T), $p.len) }
    };
    ($p:f_type, req_modules = ["\"rust_slice_mut.h\""]) => "RustForeignSlice<swig_f_type!(&T), CRustObjectMutSlice>"
        "RustForeignSlice<swig_f_type!(&T), CRustObjectMutSlice>{$p}";
    ($p:f_type, req_modules = ["\"rust_slice_mut.h\""]) <= "RustForeignSlice<swig_f_type!(&T, output), CRustObjectMutSlice>"
        "$p";
);

foreign_typemap!(
    generic_alias!(CRustSlice = swig_concat_idents!(CRustSlice, swig_i_type!(T)));
    define_c_type!(
        module = "rust_slice.h";
        #[repr(C)]
        pub struct CRustSlice!() {
            data: *const swig_i_type!(T),
            len: usize,
        }
    );
    foreigner_code!(module = "rust_slice.h";
                    r##"
#ifdef __cplusplus
#include "rust_slice_tmpl.hpp"
#endif
"##);
    ($p:r_type) <T: SwigTypeIsReprC> &[T] => CRustSlice!() {
        $out =  CRustSlice!() {
            data: $p.as_ptr(),
            len: $p.len(),
        }
    };
    ($p:r_type) <T: SwigTypeIsReprC> &[T] <= CRustSlice!() {
        assert!($p.len == 0 || !$p.data.is_null());
        $out = unsafe { ::std::slice::from_raw_parts($p.data, $p.len) }
    };
    ($p:f_type, req_modules = ["\"rust_slice.h\""]) => "RustSlice<swig_f_type!(T)>"
        "RustSlice<swig_f_type!(T)>{$p.data, $p.len}";
    ($p:f_type, req_modules = ["\"rust_slice.h\""]) <= "RustSlice<swig_f_type!(T)>"
        "$p.as_c<swig_f_type!(CRustSlice!())>()";
);
