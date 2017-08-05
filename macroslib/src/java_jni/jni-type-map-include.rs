mod foreign_types_map {
    #![foreigner_type="void"]
    #![rust_type="()"]
    #![foreigner_type="boolean"]
    #![rust_type="jboolean"]
    #![foreigner_type="byte"]
    #![rust_type="jbyte"]
    #![foreigner_type="short"]
    #![rust_type="jshort"]
    #![foreigner_type="int"]
    #![rust_type="jint"]
    #![foreigner_type="long"]
    #![rust_type="jlong"]
    #![foreigner_type="String"]
    #![rust_type="jstring"]
    #![foreigner_type="float"]
    #![rust_type="jfloat"]
    #![foreigner_type="double"]
    #![rust_type="jdouble"]
    #![foreigner_type="Object"]
    #![rust_type_not_unique="jobject"]
    #![foreigner_type="java.util.Date"]
    #![rust_type_not_unique="jobject"]
    #![foreigner_type="java.lang.String []"]
    #![rust_type_not_unique="jobjectArray"]
}

#[cfg(target_pointer_width = "32")]
unsafe fn jlong_to_pointer<T>(val: jlong) -> *mut T {
    ::std::mem::transmute::<u32, *mut T>(val as u32)
}

#[cfg(target_pointer_width = "64")]
unsafe fn jlong_to_pointer<T>(val: jlong) -> *mut T {
    ::std::mem::transmute::<jlong, *mut T>(val)
}

#[allow(dead_code)]
struct JavaString {
    string: jstring,
    chars: *const ::std::os::raw::c_char,
    env: *mut JNIEnv,
}
#[allow(dead_code)]
impl JavaString {
    fn new(env: *mut JNIEnv, js: jstring) -> JavaString {
        let chars = if !js.is_null() {
            unsafe { (**env).GetStringUTFChars.unwrap()(env, js, ::std::ptr::null_mut()) }
        } else {
            ::std::ptr::null_mut()
        };
        JavaString {
            string: js,
            chars: chars,
            env: env,
        }
    }
    fn to_str(&self) -> &str {
        if !self.chars.is_null() {
            let s = unsafe { ::std::ffi::CStr::from_ptr(self.chars) };
            s.to_str().unwrap()
        } else {
            ""
        }
    }
}

#[allow(dead_code)]
impl Drop for JavaString {
    fn drop(&mut self) {
        assert!(!self.env.is_null());
        if !self.string.is_null() {
            assert!(!self.chars.is_null());
            unsafe {
                (**self.env).ReleaseStringUTFChars.unwrap()(self.env, self.string, self.chars)
            };
            self.env = ::std::ptr::null_mut();
            self.chars = ::std::ptr::null_mut();
        }
    }
}

impl SwigDeref for JavaString {
    type Target = str;
    fn swig_deref(&self) -> &Self::Target {
        self.to_str()
    }
}

#[allow(dead_code)]
fn jni_throw(env: *mut JNIEnv, class_name: &'static str, message: &str) {
    let class_name_c = ::std::ffi::CString::new(class_name).unwrap();

    let ex_class = unsafe { (**env).FindClass.unwrap()(env, class_name_c.as_ptr()) };
    if ex_class.is_null() {
        error!("throw_exception: can not find exp class {}, msg {}",
               class_name,
               message);
        return;
    }
    let c_message = ::std::ffi::CString::new(message).unwrap();
    let res = unsafe { (**env).ThrowNew.unwrap()(env, ex_class, c_message.as_ptr()) };
    if res != 0 {
        error!("ThrowNew({}) for class {} failed", message, class_name);
    }
}

#[allow(dead_code)]
fn jni_throw_exception(env: *mut JNIEnv, message: &str) {
    jni_throw(env, "java/lang/Exception", message)
}

#[allow(dead_code)]
fn object_to_jobject<T>(obj: T, full_class_name: &str, env: *mut JNIEnv) -> jobject {
    let class_id = ::std::ffi::CString::new(full_class_name).unwrap();
    let jcls: jclass = unsafe { (**env).FindClass.unwrap()(env, class_id.as_ptr()) };
    assert!(!jcls.is_null());
    let jobj: jobject = unsafe { (**env).AllocObject.unwrap()(env, jcls) };
    assert!(!jobj.is_null());
    let field_id = ::std::ffi::CString::new("mNativeObj").unwrap();
    let type_id = ::std::ffi::CString::new("J").unwrap();
    let field_id: jfieldID =
        unsafe { (**env).GetFieldID.unwrap()(env, jcls, field_id.as_ptr(), type_id.as_ptr()) };
    assert!(!field_id.is_null());
    let b: Box<T> = Box::new(obj);
    let ret = Box::into_raw(b) as jlong;
    unsafe {
        (**env).SetLongField.unwrap()(env, jobj, field_id, ret);
        if (**env).ExceptionCheck.unwrap()(env) != 0 {
            panic!("Can not mNativeObj field: catch exception");
        }
    }
    jobj
}

#[allow(dead_code)]
fn vec_of_objects_to_jobject_array<T>(mut arr: Vec<T>,
                                      full_class_name: &str,
                                      env: *mut JNIEnv)
                                      -> jobjectArray {
    let class_id = ::std::ffi::CString::new(full_class_name).unwrap();
    let jcls: jclass = unsafe { (**env).FindClass.unwrap()(env, class_id.as_ptr()) };
    assert!(!jcls.is_null());
    //TODO: check for arr.len() -> jsize overflow
    let obj_arr: jobjectArray = unsafe {
        (**env).NewObjectArray.unwrap()(env, arr.len() as jsize, jcls, ::std::ptr::null_mut())
    };
    assert!(!obj_arr.is_null());

    let field_id = ::std::ffi::CString::new("mNativeObj").unwrap();
    let type_id = ::std::ffi::CString::new("J").unwrap();
    let field_id: jfieldID =
        unsafe { (**env).GetFieldID.unwrap()(env, jcls, field_id.as_ptr(), type_id.as_ptr()) };
    assert!(!field_id.is_null());

    for (i, r_obj) in arr.drain(..).enumerate() {
        let jobj: jobject = unsafe { (**env).AllocObject.unwrap()(env, jcls) };
        assert!(!jobj.is_null());

        let r_obj = Box::into_raw(Box::new(r_obj)) as jlong;
        unsafe {
            (**env).SetLongField.unwrap()(env, jobj, field_id, r_obj);
            if (**env).ExceptionCheck.unwrap()(env) != 0 {
                panic!("Can not mNativeObj field: catch exception");
            }
            (**env).SetObjectArrayElement.unwrap()(env, obj_arr, i as jsize, jobj);
            if (**env).ExceptionCheck.unwrap()(env) != 0 {
                panic!("SetObjectArrayElement({}) failed", i);
            }
            (**env).DeleteLocalRef.unwrap()(env, jobj);
        }
    }
    obj_arr
}

#[allow(unused_macros)]
macro_rules! jni_unpack_return {
    ($result_value:expr, $default_value:expr, $env:ident) => {
        {
            let ret = match $result_value {
                Ok(x) => x,
                Err(msg) => {
                    jni_throw_exception($env, &msg);
                    return $default_value;
                }
            };
            ret
        }
    }
}

#[allow(dead_code)]
trait SwigInto<T> {
    fn swig_into(self, env: *mut JNIEnv) -> T;
}

#[allow(dead_code)]
trait SwigFrom<T> {
    fn swig_from(T, env: *mut JNIEnv) -> Self;
}

#[allow(dead_code)]
trait SwigDeref {
    type Target: ?Sized;
    fn swig_deref(&self) -> &Self::Target;
}


impl SwigInto<bool> for jboolean {
    fn swig_into(self, _: *mut JNIEnv) -> bool {
        self != 0
    }
}

impl SwigFrom<bool> for jboolean {
    fn swig_from(x: bool, _: *mut JNIEnv) -> Self {
        if x { 1 as jboolean } else { 0 as jboolean }
    }
}

impl SwigFrom<i8> for jbyte {
    fn swig_from(x: i8, _: *mut JNIEnv) -> Self {
        x
    }
}

impl SwigInto<i8> for jbyte {
    fn swig_into(self, _: *mut JNIEnv) -> i8 {
        self
    }
}

impl SwigFrom<u8> for jshort {
    fn swig_from(x: u8, _: *mut JNIEnv) -> Self {
        x as jshort
    }
}

impl SwigInto<u8> for jshort {
    fn swig_into(self, _: *mut JNIEnv) -> u8 {
        if self < 0 || self > (::std::u8::MAX as jshort) {
            panic!("Expect self from 0 to {}, got {}", ::std::u8::MAX, self);
        }
        self as u8
    }
}

impl SwigInto<i16> for jshort {
    fn swig_into(self, _: *mut JNIEnv) -> i16 {
        self
    }
}

impl SwigFrom<i16> for jshort {
    fn swig_from(x: i16, _: *mut JNIEnv) -> Self {
        x
    }
}

impl SwigFrom<u16> for jint {
    fn swig_from(x: u16, _: *mut JNIEnv) -> Self {
        x as jint
    }
}

impl SwigInto<u16> for jint {
    fn swig_into(self, _: *mut JNIEnv) -> u16 {
        if self < 0 || self > (::std::u16::MAX as jint) {
            panic!("Expect self from 0 to {}, got {}", ::std::u16::MAX, self);
        }
        self as u16
    }
}

impl SwigInto<i32> for jint {
    fn swig_into(self, _: *mut JNIEnv) -> i32 {
        self
    }
}

impl SwigFrom<i32> for jint {
    fn swig_from(x: i32, _: *mut JNIEnv) -> Self {
        x
    }
}

impl SwigFrom<u32> for jlong {
    fn swig_from(x: u32, _: *mut JNIEnv) -> Self {
        x as jlong
    }
}

impl SwigInto<u32> for jlong {
    fn swig_into(self, _: *mut JNIEnv) -> u32 {
        if self < 0 || self > (::std::u32::MAX as jlong) {
            panic!("Expect self from 0 to {}, got {}", ::std::u32::MAX, self);
        }
        self as u32
    }
}

impl SwigInto<i64> for jlong {
    fn swig_into(self, _: *mut JNIEnv) -> i64 {
        self
    }
}

impl SwigFrom<i64> for jlong {
    fn swig_from(x: i64, _: *mut JNIEnv) -> Self {
        x
    }
}

impl SwigInto<u64> for jlong {
    fn swig_into(self, _: *mut JNIEnv) -> u64 {
        if self < 0 {
            panic!("Expect self to be positive, got {}", self);
        }
        self as u64
    }
}

impl SwigFrom<u64> for jlong {
    fn swig_from(x: u64, _: *mut JNIEnv) -> Self {
        if (::std::i64::MAX as u64) < x {
            error!("u64->jlong type overflow: {}", x);
            ::std::i64::MAX
        } else {
            x as i64
        }
    }
}

impl SwigInto<f32> for jfloat {
    fn swig_into(self, _: *mut JNIEnv) -> f32 {
        self
    }
}

impl SwigFrom<f32> for jfloat {
    fn swig_from(x: f32, _: *mut JNIEnv) -> Self {
        x
    }
}

impl SwigInto<f64> for jdouble {
    fn swig_into(self, _: *mut JNIEnv) -> f64 {
        self
    }
}

impl SwigFrom<f64> for jdouble {
    fn swig_from(x: f64, _: *mut JNIEnv) -> Self {
        x
    }
}

impl<'a> SwigFrom<&'a str> for jstring {
    fn swig_from(x: &'a str, env: *mut JNIEnv) -> Self {
        let x = ::std::ffi::CString::new(x).unwrap();
        unsafe { (**env).NewStringUTF.unwrap()(env, x.as_ptr()) }
    }
}

impl SwigInto<JavaString> for jstring {
    fn swig_into(self, env: *mut JNIEnv) -> JavaString {
        JavaString::new(env, self)
    }
}

impl SwigFrom<String> for jstring {
    fn swig_from(x: String, env: *mut JNIEnv) -> Self {
        let x = x.into_bytes();
        let x = unsafe { ::std::ffi::CString::from_vec_unchecked(x) };
        unsafe { (**env).NewStringUTF.unwrap()(env, x.as_ptr()) }
    }
}

#[to_foreigner_hint = "java.util.Date"]
impl SwigFrom<SystemTime> for jobject {
    fn swig_from(x: SystemTime, env: *mut JNIEnv) -> Self {
        let since_unix_epoch = x.duration_since(::std::time::UNIX_EPOCH).unwrap();
        let mills: jlong = (since_unix_epoch.as_secs() * 1_000 +
                            (since_unix_epoch.subsec_nanos() / 1_000_000) as u64) as
                           jlong;
        let class_name_c = ::std::ffi::CString::new("java/util/Date").unwrap();
        let date_class: jclass = unsafe { (**env).FindClass.unwrap()(env, class_name_c.as_ptr()) };
        assert!(!date_class.is_null());
        let init_name_c = ::std::ffi::CString::new("<init>").unwrap();
        let method_args_c = ::std::ffi::CString::new("(J)V").unwrap();
        let init: jmethodID = unsafe {
            (**env).GetMethodID.unwrap()(env,
                                         date_class,
                                         init_name_c.as_ptr(),
                                         method_args_c.as_ptr())
        };
        assert!(!init.is_null());
        let x = unsafe { (**env).NewObject.unwrap()(env, date_class, init, mills) };
        assert!(!x.is_null());
        x
    }
}

impl SwigInto<usize> for i64 {
    fn swig_into(self, _: *mut JNIEnv) -> usize {
        if self < 0 {
            panic!("{}:{} expect self to be positive, got {}",
                   file!(),
                   line!(),
                   self);
        } else if (self as u64) > (::std::usize::MAX as u64) {
            panic!("{}:{} too big value for usize {}", self);
        } else {
            self as usize
        }
    }
}

// &str -> &Path
impl<'a> SwigInto<&'a Path> for &'a str {
    fn swig_into(self, _: *mut JNIEnv) -> &'a Path {
        Path::new(self)
    }
}

// Vec<String> -> jobjectArray
#[to_foreigner_hint = "java.lang.String []"]
impl SwigInto<jobjectArray> for Vec<String> {
    fn swig_into(mut self, env: *mut JNIEnv) -> jobjectArray {
        let class_id = unsafe {
            ::std::ffi::CStr::from_ptr(concat!("java/lang/String", "\0").as_ptr() as
                                       *const ::std::os::raw::c_char)
        };
        let jcls: jclass = unsafe { (**env).FindClass.unwrap()(env, class_id.as_ptr()) };
        assert!(!jcls.is_null());
        let obj_arr: jobjectArray = unsafe {
            (**env).NewObjectArray.unwrap()(env, self.len() as jsize, jcls, ::std::ptr::null_mut())
        };
        assert!(!obj_arr.is_null());
        for (i, r_str) in self.drain(..).enumerate() {
            let jstr: jstring = jstring::swig_from(r_str, env);
            assert!(!jstr.is_null());

            unsafe {
                (**env).SetObjectArrayElement.unwrap()(env, obj_arr, i as jsize, jstr);
                if (**env).ExceptionCheck.unwrap()(env) != 0 {
                    panic!("SetObjectArrayElement({}) failed", i);
                }
                (**env).DeleteLocalRef.unwrap()(env, jstr);
            }
        }
        obj_arr
    }
}
