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
}

#[allow(unused_macros)]
macro_rules! swig_c_str {
    ($lit:expr) => {
        concat!($lit, "\0").as_ptr()
            as *const ::std::os::raw::c_char
    }
}

#[allow(dead_code)]
trait SwigForeignClass {
    fn c_class_name() -> *const ::std::os::raw::c_char;
    fn box_object(x: Self) -> *const Self;
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
