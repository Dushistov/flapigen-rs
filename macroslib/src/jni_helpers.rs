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
    env: *mut JNIEnv
}
#[allow(dead_code)]
impl JavaString {
    fn new(env: *mut JNIEnv, js: jstring) -> JavaString {
        let chars = unsafe { (**env).GetStringUTFChars.unwrap()(env, js, ::std::ptr::null_mut()) };
        JavaString{string: js, chars: chars, env: env}
    }
    fn to_str(&self) -> &str {
        let s = unsafe { ::std::ffi::CStr::from_ptr(self.chars) };
        s.to_str().unwrap()
    }
}

#[allow(dead_code)]
impl Drop for JavaString {
    fn drop(&mut self) {
        assert!(self.env != ::std::ptr::null_mut() && self.chars != ::std::ptr::null_mut());
        unsafe { (**self.env).ReleaseStringUTFChars.unwrap()(self.env, self.string, self.chars) };
        self.env = ::std::ptr::null_mut();
        self.chars = ::std::ptr::null_mut();
    }
}

#[allow(dead_code)]
fn jni_throw(env: *mut JNIEnv, class_name: &'static str, message: &str) {
    let class_name_c = ::std::ffi::CString::new(class_name).unwrap();

    let ex_class = unsafe {
        (**env).FindClass.unwrap()(env, class_name_c.as_ptr())
    };
    if ex_class.is_null() {
        error!("throw_exception: can not find exp class {}, msg {}", class_name, message);
        return;
    }
    let c_message = ::std::ffi::CString::new(message).unwrap();
    let res = unsafe {
        (**env).ThrowNew.unwrap()(env, ex_class, c_message.as_ptr())
    };
    if res != 0 {
        error!("ThrowNew({}) for class {} failed", message, class_name);
    }
}

#[allow(dead_code)]
fn jni_throw_exception(env: *mut JNIEnv, message: &str) {
    jni_throw(env, "java/lang/Exception", message)
}

#[allow(dead_code)]
trait SwigInto<T> {
    fn swig_into(self, env: *mut JNIEnv) -> T;
}

#[allow(dead_code)]
trait SwigFrom<T> {
    fn swig_from(T, env: *mut JNIEnv) -> Self;
}
