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
    #![swig_foreigner_type = "struct RustStrView"]
    #![swig_rust_type = "RustStrView"]
    #![swig_foreigner_type = "struct RustVecBytes"]
    #![swig_rust_type = "RustVecBytes"]
    #![swig_foreigner_type = "struct CRustString"]
    #![swig_rust_type = "CRustString"]
    #![swig_foreigner_type = "struct CResultObjectString"]
    #![swig_rust_type = "CResultObjectString"]
    #![swig_foreigner_type = "struct CRustSliceU32"]
    #![swig_rust_type = "CRustSliceU32"]
}

#[allow(unused_macros)]
macro_rules! swig_c_str {
    ($lit:expr) => {
        concat!($lit, "\0").as_ptr()
            as *const ::std::os::raw::c_char
    }
}

#[allow(dead_code)]
pub trait SwigForeignClass {
    fn c_class_name() -> *const ::std::os::raw::c_char;
    fn box_object(x: Self) -> *mut ::std::os::raw::c_void;
}

#[allow(dead_code)]
#[swig_code = "let mut {to_var}: {to_var_type} = {from_var}.swig_into();"]
trait SwigInto<T> {
    fn swig_into(self) -> T;
}

#[allow(dead_code)]
#[swig_code = "let mut {to_var}: {to_var_type} = <{to_var_type}>::swig_from({from_var});"]
trait SwigFrom<T> {
    fn swig_from(T) -> Self;
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

impl SwigInto<bool> for ::std::os::raw::c_char {
    fn swig_into(self) -> bool {
        self != 0
    }
}

impl SwigFrom<bool> for ::std::os::raw::c_char {
    fn swig_from(x: bool) -> Self {
        if x {
            1
        } else {
            0
        }
    }
}

impl<'a> SwigInto<&'a ::std::ffi::CStr> for *const ::std::os::raw::c_char {
    fn swig_into(self) -> &'a ::std::ffi::CStr {
        assert!(!self.is_null());
        unsafe { ::std::ffi::CStr::from_ptr(self) }
    }
}

impl<'a> SwigDeref for &'a ::std::ffi::CStr {
    type Target = str;
    fn swig_deref(&self) -> &Self::Target {
        self.to_str().expect("wrong utf-8")
    }
}

#[allow(dead_code)]
#[repr(C)]
pub struct RustStrView {
    data: *const ::std::os::raw::c_char,
    len: u32,
}

impl<'a> SwigFrom<&'a str> for RustStrView {
    fn swig_from(s: &'a str) -> RustStrView {
        assert!((s.len() as u64) <= (::std::u32::MAX as u64));
        RustStrView {
            data: s.as_ptr() as *const ::std::os::raw::c_char,
            len: s.len() as u32,
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

#[swig_to_foreigner_hint = "T"]
impl<T: SwigForeignClass> SwigFrom<T> for *mut ::std::os::raw::c_void {
    fn swig_from(x: T) -> Self {
        <T>::box_object(x)
    }
}

#[swig_to_foreigner_hint = "T"]
impl<'a, T: SwigForeignClass> SwigFrom<&'a T> for *const ::std::os::raw::c_void {
    fn swig_from(x: &'a T) -> Self {
        (x as *const T) as *const ::std::os::raw::c_void
    }
}

#[allow(dead_code)]
#[repr(C)]
pub struct RustVecBytes {
    data: *const u8,
    len: u32,
    capacity: u32,
}

impl SwigFrom<Vec<u8>> for RustVecBytes {
    fn swig_from(mut v: Vec<u8>) -> RustVecBytes {
        let p = v.as_mut_ptr();
        let len = v.len();
        let cap = v.capacity();
        ::std::mem::forget(v);
        //todo check that u32 <-> usize safe, or may be use u64?
        RustVecBytes {
            data: p,
            len: len as u32,
            capacity: cap as u32,
        }
    }
}

#[allow(private_no_mangle_fns)]
#[no_mangle]
pub extern "C" fn rust_vec_bytes_free(v: RustVecBytes) {
    //todo check that u32 <-> usize safe, or may be use u64?
    let v = unsafe { Vec::from_raw_parts(v.data as *mut u8, v.len as usize, v.capacity as usize) };
    drop(v);
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
    len: u32,
    capacity: u32,
}

#[allow(private_no_mangle_fns)]
#[no_mangle]
pub extern "C" fn crust_string_free(x: CRustString) {
    let s =
        unsafe { String::from_raw_parts(x.data as *mut u8, x.len as usize, x.capacity as usize) };
    drop(s);
}

#[allow(dead_code)]
impl CRustString {
    pub fn from_string(s: String) -> CRustString {
        let data = s.as_ptr() as *const ::std::os::raw::c_char;
        assert!((s.len() as u64) <= (::std::u32::MAX as u64));
        let len = s.len() as u32;
        assert!((s.capacity() as u64) <= (::std::u32::MAX as u64));
        let capacity = s.capacity() as u32;
        ::std::mem::forget(s);
        CRustString {
            data,
            len,
            capacity,
        }
    }
}

impl SwigFrom<String> for CRustString {
    fn swig_from(s: String) -> CRustString {
        CRustString::from_string(s)
    }
}

#[swig_to_foreigner_hint = "T"]
impl<T: SwigForeignClass> SwigFrom<Option<T>> for *mut ::std::os::raw::c_void {
    fn swig_from(x: Option<T>) -> Self {
        match x {
            Some(x) => <T>::box_object(x),
            None => ::std::ptr::null_mut(),
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
pub struct CRustSliceU32 {
    data: *const u32,
    len: usize,
}

impl<'a> SwigInto<CRustSliceU32> for &'a [u32] {
    fn swig_into(self) -> CRustSliceU32 {
        CRustSliceU32 {
            data: self.as_ptr(),
            len: self.len(),
        }
    }
}
