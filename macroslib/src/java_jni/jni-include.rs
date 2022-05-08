mod swig_foreign_types_map {
    #![swig_foreigner_type = "Object"]
    #![swig_rust_type_not_unique = "jobject"]
    #![swig_foreigner_type = "Object []"]
    #![swig_rust_type_not_unique = "jobjectArray"]
}

#[allow(dead_code)]
mod internal_aliases {
    use super::*;
    pub type JStringOptStr = jstring;
    pub type JOptionalInt = jobject;
    pub type JInteger = jobject;
    pub type JByte = jobject;
    pub type JShort = jobject;
    pub type JFloat = jobject;
    pub type JDouble = jobject;
    pub type JOptionalDouble = jobject;
    pub type JLong = jobject;
    pub type JOptionalLong = jobject;
    #[repr(transparent)]
    pub struct JForeignObjectsArray<T: SwigForeignClass> {
        pub(crate) inner: jobjectArray,
        pub(crate) _marker: ::std::marker::PhantomData<T>,
    }
    pub type JStringPath = jstring;
    pub type JStringObjectsArray = jobjectArray;
}

/// Default JNI_VERSION
const SWIG_JNI_VERSION: jint = JNI_VERSION_1_6 as jint;

/// Marker for what to cache in JNI_OnLoad
#[allow(unused_macros)]
macro_rules! swig_jni_find_class {
    ($id:ident, $path:expr) => {
        unsafe { $id }
    };
    ($id:ident, $path:expr,) => {
        unsafe { $id }
    };
}
#[allow(unused_macros)]
macro_rules! swig_jni_get_method_id {
    ($global_id:ident, $class_id:ident, $name:expr, $sig:expr) => {
        unsafe { $global_id }
    };
    ($global_id:ident, $class_id:ident, $name:expr, $sig:expr,) => {
        unsafe { $global_id }
    };
}
#[allow(unused_macros)]
macro_rules! swig_jni_get_static_method_id {
    ($global_id:ident, $class_id:ident, $name:expr, $sig:expr) => {
        unsafe { $global_id }
    };
    ($global_id:ident, $class_id:ident, $name:expr, $sig:expr,) => {
        unsafe { $global_id }
    };
}
#[allow(unused_macros)]
macro_rules! swig_jni_get_field_id {
    ($global_id:ident, $class_id:ident, $name:expr, $sig:expr) => {
        unsafe { $global_id }
    };
    ($global_id:ident, $class_id:ident, $name:expr, $sig:expr,) => {
        unsafe { $global_id }
    };
}
#[allow(unused_macros)]
macro_rules! swig_jni_get_static_field_id {
    ($global_id:ident, $class_id:ident, $name:expr, $sig:expr) => {
        unsafe { $global_id }
    };
    ($global_id:ident, $class_id:ident, $name:expr, $sig:expr,) => {
        unsafe { $global_id }
    };
}

#[allow(dead_code)]
#[swig_code = "let mut {to_var}: {to_var_type} = {from_var}.swig_into(env);"]
trait SwigInto<T> {
    fn swig_into(self, env: *mut JNIEnv) -> T;
}

#[allow(dead_code)]
#[swig_code = "let mut {to_var}: {to_var_type} = <{to_var_type}>::swig_from({from_var}, env);"]
trait SwigFrom<T> {
    fn swig_from(_: T, env: *mut JNIEnv) -> Self;
}

#[allow(unused_macros)]
macro_rules! swig_c_str {
    ($lit:expr) => {
        concat!($lit, "\0").as_ptr() as *const ::std::os::raw::c_char
    };
}

#[allow(unused_macros)]
macro_rules! swig_assert_eq_size {
    ($x:ty, $($xs:ty),+ $(,)*) => {
        $(let _ = ::std::mem::transmute::<$x, $xs>;)+
    };
}

#[cfg(target_pointer_width = "32")]
pub unsafe fn jlong_to_pointer<T>(val: jlong) -> *mut T {
    (val as u32) as *mut T
}

#[cfg(target_pointer_width = "64")]
pub unsafe fn jlong_to_pointer<T>(val: jlong) -> *mut T {
    val as *mut T
}

foreign_typemap!(
    (r_type) ();
    (f_type) "void";
);

foreign_typemap!(
    (r_type) jbyte;
    (f_type) "byte";
);

foreign_typemap!(
    (r_type) jshort;
    (f_type) "short";
);

foreign_typemap!(
    (r_type) jint;
    (f_type) "int";
);

//ANCHOR: foreign_typemap_define_jlong
foreign_typemap!(
    (r_type) jlong;
    (f_type) "long";
);
//ANCHOR_END: foreign_typemap_define_jlong

foreign_typemap!(
    (r_type) jfloat;
    (f_type) "float";
);

foreign_typemap!(
    (r_type) jdouble;
    (f_type) "double";
);

foreign_typemap!(
    (r_type) jstring;
    (f_type, option = "NoNullAnnotations") "String";
    (f_type, option = "NullAnnotations") "@NonNull String";
);

#[allow(dead_code)]
pub trait SwigForeignClass {
    type PointedType;
    fn jni_class() -> jclass;
    fn jni_class_pointer_field() -> jfieldID;
    fn box_object(x: Self) -> jlong;
    fn unbox_object(x: jlong) -> Self;
    fn to_pointer(x: jlong) -> ::std::ptr::NonNull<Self::PointedType>;
}

#[allow(dead_code)]
pub trait SwigForeignCLikeEnum {
    fn as_jint(&self) -> jint;
    /// # Panics
    /// Panics on error
    fn from_jint(_: jint) -> Self;
}

#[allow(dead_code)]
pub struct JavaString {
    string: jstring,
    chars: *const ::std::os::raw::c_char,
    env: *mut JNIEnv,
}
#[allow(dead_code)]
impl JavaString {
    pub fn new(env: *mut JNIEnv, js: jstring) -> JavaString {
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
    pub fn to_str(&self) -> &str {
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

foreign_typemap!(
    ($p:r_type) JavaString => &str {
        $out = $p.to_str();
    };
);

#[allow(dead_code)]
struct JavaCallback {
    java_vm: *mut JavaVM,
    this: jobject,
    methods: Vec<jmethodID>,
}

/// According to JNI spec it should be safe to
/// pass pointer to JavaVm and jobject (global) across threads
unsafe impl Send for JavaCallback {}

#[allow(dead_code)]
struct JniEnvHolder<'a> {
    env: Option<*mut JNIEnv>,
    callback: &'a JavaCallback,
    need_detach: bool,
}

#[allow(dead_code)]
impl<'a> Drop for JniEnvHolder<'a> {
    fn drop(&mut self) {
        if self.need_detach {
            let res = unsafe {
                (**self.callback.java_vm).DetachCurrentThread.unwrap()(self.callback.java_vm)
            };
            if res != 0 {
                log::error!("JniEnvHolder: DetachCurrentThread failed: {}", res);
            }
        }
    }
}

#[allow(dead_code)]
impl JavaCallback {
    fn new(obj: jobject, env: *mut JNIEnv) -> JavaCallback {
        let mut java_vm: *mut JavaVM = ::std::ptr::null_mut();
        let ret = unsafe { (**env).GetJavaVM.unwrap()(env, &mut java_vm) };
        assert_eq!(0, ret, "GetJavaVm failed");
        let global_obj = unsafe { (**env).NewGlobalRef.unwrap()(env, obj) };
        assert!(!global_obj.is_null());
        JavaCallback {
            java_vm,
            this: global_obj,
            methods: Vec::new(),
        }
    }

    fn get_jni_env(&self) -> JniEnvHolder {
        assert!(!self.java_vm.is_null());
        let mut env: *mut JNIEnv = ::std::ptr::null_mut();

        let res = unsafe {
            (**self.java_vm).GetEnv.unwrap()(
                self.java_vm,
                (&mut env) as *mut *mut JNIEnv as *mut *mut ::std::os::raw::c_void,
                SWIG_JNI_VERSION,
            )
        };
        if res == (JNI_OK as jint) {
            return JniEnvHolder {
                env: Some(env),
                callback: self,
                need_detach: false,
            };
        }
        if res != (JNI_EDETACHED as jint) {
            panic!("get_jni_env: GetEnv return error `{}`", res);
        }

        // AttachCurrentThread for Android and other JNI
        // has different signatures, second argument has type
        // *mut *mut JNIEnv instead of *mut *mut c_void
        trait ConvertPtr<T> {
            fn convert_ptr(self) -> T;
        }

        impl ConvertPtr<*mut *mut ::std::os::raw::c_void> for *mut *mut JNIEnv {
            fn convert_ptr(self) -> *mut *mut ::std::os::raw::c_void {
                self as *mut *mut ::std::os::raw::c_void
            }
        }

        impl ConvertPtr<*mut *mut JNIEnv> for *mut *mut JNIEnv {
            fn convert_ptr(self) -> *mut *mut JNIEnv {
                self
            }
        }

        let res = unsafe {
            (**self.java_vm).AttachCurrentThread.unwrap()(
                self.java_vm,
                (&mut env as *mut *mut JNIEnv).convert_ptr(),
                ::std::ptr::null_mut(),
            )
        };
        if res != 0 {
            log::error!(
                "JavaCallback::get_jnienv: AttachCurrentThread failed: {}",
                res
            );
            JniEnvHolder {
                env: None,
                callback: self,
                need_detach: false,
            }
        } else {
            assert!(!env.is_null());
            JniEnvHolder {
                env: Some(env),
                callback: self,
                need_detach: true,
            }
        }
    }
}

#[allow(dead_code)]
impl Drop for JavaCallback {
    fn drop(&mut self) {
        let env = self.get_jni_env();
        if let Some(env) = env.env {
            assert!(!env.is_null());
            unsafe { (**env).DeleteGlobalRef.unwrap()(env, self.this) };
        } else {
            log::error!("JavaCallback::drop failed, can not get JNIEnv");
        }
    }
}

#[allow(dead_code)]
fn jni_throw(env: *mut JNIEnv, ex_class: jclass, message: &str) {
    let c_message = ::std::ffi::CString::new(message).unwrap();
    let res = unsafe { (**env).ThrowNew.unwrap()(env, ex_class, c_message.as_ptr()) };
    if res != 0 {
        log::error!(
            "JNI ThrowNew({}) failed for class {:?} failed",
            message,
            ex_class
        );
    }
}

#[allow(dead_code)]
fn jni_throw_exception(env: *mut JNIEnv, message: &str) {
    let exception_class = swig_jni_find_class!(JAVA_LANG_EXCEPTION, "java/lang/Exception");
    jni_throw(env, exception_class, message)
}

#[allow(dead_code)]
fn object_to_jobject<T: SwigForeignClass>(env: *mut JNIEnv, obj: T) -> jobject {
    let jcls = <T>::jni_class();
    assert!(!jcls.is_null());
    let field_id = <T>::jni_class_pointer_field();
    assert!(!field_id.is_null());

    let jobj: jobject = unsafe { (**env).AllocObject.unwrap()(env, jcls) };
    assert!(!jobj.is_null(), "object_to_jobject: AllocObject failed");

    let ret: jlong = <T>::box_object(obj);
    unsafe {
        (**env).SetLongField.unwrap()(env, jobj, field_id, ret);
        if (**env).ExceptionCheck.unwrap()(env) != 0 {
            panic!("object_to_jobject: Can not set mNativeObj field: catch exception");
        }
    }
    jobj
}

foreign_typemap!(
    ($p:r_type) <T: SwigForeignClass> Vec<T> => internal_aliases::JForeignObjectsArray<T> {
        $out = vec_of_objects_to_jobject_array(env, $p);
    };
    ($p:f_type, option = "NoNullAnnotations") => "swig_f_type!(T) []";
    ($p:f_type, option = "NullAnnotations") => "@NonNull swig_f_type!(T, NoNullAnnotations) []";
);

#[allow(dead_code)]
fn jobject_array_to_vec_of_objects<T: SwigForeignClass + Clone>(
    env: *mut JNIEnv,
    arr: internal_aliases::JForeignObjectsArray<T>,
) -> Vec<T> {
    let field_id = <T>::jni_class_pointer_field();
    assert!(!field_id.is_null());
    let length = unsafe { (**env).GetArrayLength.unwrap()(env, arr.inner) };
    let len = <usize as ::std::convert::TryFrom<jsize>>::try_from(length)
        .expect("invalid jsize, in jsize => usize conversion");
    let mut result = Vec::with_capacity(len);
    for i in 0..length {
        let native: &mut T = unsafe {
            let obj = (**env).GetObjectArrayElement.unwrap()(env, arr.inner, i);
            if (**env).ExceptionCheck.unwrap()(env) != 0 {
                panic!("Failed to retrieve element {} from this `jobjectArray'", i);
            }
            let ptr = (**env).GetLongField.unwrap()(env, obj, field_id);
            let native = (jlong_to_pointer(ptr) as *mut T).as_mut().unwrap();
            (**env).DeleteLocalRef.unwrap()(env, obj);
            native
        };
        result.push(native.clone());
    }

    result
}
//ANCHOR: foreign_typemap_generic_example
foreign_typemap!(
    ($p:r_type) <T: SwigForeignClass + Clone> Vec<T> <= internal_aliases::JForeignObjectsArray<T> {
        $out = jobject_array_to_vec_of_objects(env, $p);
    };
    ($p:f_type, option = "NoNullAnnotations") <= "swig_f_type!(T) []";
    ($p:f_type, option = "NullAnnotations")
                  <= "@NonNull swig_f_type!(T, NoNullAnnotations) []";
);
//ANCHOR_END: foreign_typemap_generic_example

#[allow(dead_code)]
fn vec_of_objects_to_jobject_array<T: SwigForeignClass>(
    env: *mut JNIEnv,
    mut arr: Vec<T>,
) -> internal_aliases::JForeignObjectsArray<T> {
    let jcls: jclass = <T>::jni_class();
    assert!(!jcls.is_null());
    let arr_len = <jsize as ::std::convert::TryFrom<usize>>::try_from(arr.len())
        .expect("invalid usize, in usize => to jsize conversion");
    let obj_arr: jobjectArray =
        unsafe { (**env).NewObjectArray.unwrap()(env, arr_len, jcls, ::std::ptr::null_mut()) };
    assert!(!obj_arr.is_null());

    let field_id = <T>::jni_class_pointer_field();
    assert!(!field_id.is_null());

    for (i, r_obj) in arr.drain(..).enumerate() {
        let jobj: jobject = unsafe { (**env).AllocObject.unwrap()(env, jcls) };
        assert!(!jobj.is_null());

        let r_obj: jlong = <T>::box_object(r_obj);
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
    internal_aliases::JForeignObjectsArray {
        inner: obj_arr,
        _marker: ::std::marker::PhantomData,
    }
}

#[allow(dead_code)]
trait JniInvalidValue {
    fn jni_invalid_value() -> Self;
}

impl<T> JniInvalidValue for *const T {
    fn jni_invalid_value() -> Self {
        ::std::ptr::null()
    }
}

impl<T> JniInvalidValue for *mut T {
    fn jni_invalid_value() -> Self {
        ::std::ptr::null_mut()
    }
}

impl JniInvalidValue for () {
    fn jni_invalid_value() {}
}

impl<T: SwigForeignClass> JniInvalidValue for internal_aliases::JForeignObjectsArray<T> {
    fn jni_invalid_value() -> Self {
        Self {
            inner: ::std::ptr::null_mut(),
            _marker: ::std::marker::PhantomData,
        }
    }
}

macro_rules! impl_jni_jni_invalid_value {
    ($($type:ty)*) => ($(
        impl JniInvalidValue for $type {
            fn jni_invalid_value() -> Self {
                <$type>::default()
            }
        }
    )*)
}

impl_jni_jni_invalid_value! {
    jbyte jshort jint jlong jfloat jdouble jboolean
}

foreign_typemap!(
    ($p:r_type) <T> Result<T, &str> => swig_i_type!(T) {
        $out = match $p {
            Ok(x) => {
                swig_from_rust_to_i_type!(T, x, ret)
                ret
            }
            Err(msg) => {
                jni_throw_exception(env, msg);
                return <swig_i_type!(T)>::jni_invalid_value();
            }
        };
    };
    ($p:f_type, unique_prefix="/*Result<swig_subst_type!(T), &str>*/") => "/*Result<swig_subst_type!(T), &str>*/swig_f_type!(T)"
        "swig_foreign_from_i_type!(T, $p)";
);

foreign_typemap!(
    ($p:r_type) <T> Result<T, String> => swig_i_type!(T) {
        $out = match $p {
            Ok(x) => {
                swig_from_rust_to_i_type!(T, x, ret)
                ret
            }
            Err(msg) => {
                jni_throw_exception(env, &msg);
                return <swig_i_type!(T)>::jni_invalid_value();
            }
        };
    };
    ($p:f_type, unique_prefix="/*Result<swig_subst_type!(T), String>*/") => "/*Result<swig_subst_type!(T), String>*/swig_f_type!(T)"
        "swig_foreign_from_i_type!(T, $p)";
);

foreign_typemap!(
    ($p:r_type) bool => jboolean {
        $out = if $p { 1 as jboolean } else { 0 as jboolean };
    };
    ($p:f_type) => "boolean";
    ($p:r_type) bool <= jboolean {
        $out = $p != 0;
    };
    ($p:f_type) <= "boolean";
);

foreign_typemap!(
    ($p:r_type) SystemTime => jlong {
        let since_unix_epoch = $p
            .duration_since(::std::time::UNIX_EPOCH)
            .expect("SystemTime to Unix time conv. error");
        $out = <i64 as ::std::convert::TryFrom<u64>>::try_from(
            since_unix_epoch.as_secs() * 1_000 + u64::from(since_unix_epoch.subsec_millis()),
        )
        .expect("SystemTime: milleseconds u64 to i64 convert error");
    };
    ($p:f_type, option = "NoNullAnnotations") => "java.util.Date" "$out = new java.util.Date($p);";
    ($p:f_type, option = "NullAnnotations") => "@NonNull java.util.Date" "$out = new java.util.Date($p);";
);

foreign_typemap!(
    ($p:r_type) jbyte => i8 {
        $out = $p;
    };
    ($p:r_type) jbyte <= i8 {
        $out = $p;
    };
);

foreign_typemap!(
    ($p:r_type) u8 => jshort {
        $out = jshort::from($p);
    };
);
foreign_typemap!(
    ($p:r_type) u8 <= jshort {
        $out = <u8 as ::std::convert::TryFrom<jshort>>::try_from($p)
            .expect("invalid jshort, in jshort => u8 conversion");
    };
);

foreign_typemap!(
    ($p:r_type) i16 => jshort {
        $out = $p;
    };
    ($p:r_type) i16 <= jshort {
        $out = $p;
    };
);

foreign_typemap!(
    ($p:r_type) u16 => jint {
        $out = jint::from($p);
    };
    ($p:r_type) u16 <= jint {
        $out = <u16 as ::std::convert::TryFrom<jint>>::try_from($p)
            .expect("invalid jint, in jint => u16 conversion");
    };
);

foreign_typemap!(
    ($p:r_type) jint => i32 {
        $out = $p;
    };
    ($p:r_type) jint <= i32 {
        $out = $p;
    };
);

foreign_typemap!(
    ($p:r_type) u32 => jlong {
        $out = jlong::from($p);
    };
    ($p:r_type) u32 <= jlong {
        $out = <u32 as ::std::convert::TryFrom<jlong>>::try_from($p)
            .expect("invalid jlong, in jlong => u32 conversion");
    };
);

foreign_typemap!(
    ($p:r_type) i64 => jlong {
        $out = $p;
    };
    ($p:r_type) i64 <= jlong {
        $out = $p;
    };
);

foreign_typemap!(
    ($p:r_type) u64 => jlong {
        $out = <jlong as ::std::convert::TryFrom<u64>>::try_from($p)
            .expect("invalid u64, in u64 => jlong conversion");
    };
    ($p:r_type) u64 <= jlong {
        $out = <u64 as ::std::convert::TryFrom<jlong>>::try_from($p)
            .expect("invalid jlong, in jlong => u64 conversion");
    };
);

#[allow(dead_code)]
pub fn u64_to_jlong_checked(x: u64) -> jlong {
    <jlong as ::std::convert::TryFrom<u64>>::try_from(x)
        .expect("invalid u64, in u64 => jlong conversion")
}

foreign_typemap!(
    ($p:r_type) f32 => jfloat {
        $out = $p;
    };
    ($p:r_type) f32 <= jfloat {
        $out = $p;
    };
);

foreign_typemap!(
    ($p:r_type) f64 => jdouble {
        $out = $p;
    };
    ($p:r_type) f64 <= jdouble {
        $out = $p;
    };
);

foreign_typemap!(
    ($p:r_type) &str => jstring {
        $out = {
            let x = ::std::ffi::CString::new($p).unwrap();
            unsafe { (**env).NewStringUTF.unwrap()(env, x.as_ptr()) }
        };
    };
);

foreign_typemap!(
    ($p:r_type) JavaString <= jstring {
        $out = JavaString::new(env, $p);
    };
);

foreign_typemap!(
    ($p:r_type) String => jstring {
        $out = from_std_string_jstring($p, env);
    };
);

#[allow(dead_code)]
fn from_std_string_jstring(x: String, env: *mut JNIEnv) -> jstring {
    let x = x.into_bytes();
    unsafe {
        let x = ::std::ffi::CString::from_vec_unchecked(x);
        (**env).NewStringUTF.unwrap()(env, x.as_ptr())
    }
}

foreign_typemap!(
    ($p:r_type) usize <= jlong {
        $out = <usize as ::std::convert::TryFrom<jlong>>::try_from($p)
            .expect("invalid jlong, in jlong => usize conversion");
    };
);

foreign_typemap!(
    ($p:r_type) &Path <= internal_aliases::JStringPath {
        let jstr = JavaString::new(env, $p);
        $out = Path::new(jstr.to_str());
    };
    ($p:f_type, option = "NoNullAnnotations", unique_prefix="/*Path*/") <= "/*Path*/String";
    ($p:f_type, option = "NullAnnotations", unique_prefix="/*Path*/") <= "/*Path*/@NonNull String";
);

#[allow(dead_code)]
fn vec_string_to_jobject_array(mut arr: Vec<String>, env: *mut JNIEnv) -> jobjectArray {
    let jcls: jclass = swig_jni_find_class!(JAVA_LANG_STRING, "java/lang/String");
    assert!(!jcls.is_null());
    let obj_arr: jobjectArray = unsafe {
        (**env).NewObjectArray.unwrap()(env, arr.len() as jsize, jcls, ::std::ptr::null_mut())
    };
    assert!(!obj_arr.is_null());
    for (i, r_str) in arr.drain(..).enumerate() {
        let jstr: jstring = from_std_string_jstring(r_str, env);
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

foreign_typemap!(
    ($p:r_type) Vec<String> => internal_aliases::JStringObjectsArray {
        $out = vec_string_to_jobject_array($p, env);
    };
    ($p:f_type, option = "NoNullAnnotations") => "java.lang.String []";
    ($p:f_type, option = "NullAnnotations") => "@NonNull java.lang.String []";
);

macro_rules! define_array_handling_code {
    ($([jni_arr_type = $jni_arr_type:ident,
        rust_arr_wrapper = $rust_arr_wrapper:ident,
        jni_get_array_elements = $jni_get_array_elements:ident,
        jni_elem_type = $jni_elem_type:ident,
        rust_elem_type = $rust_elem_type:ident,
        jni_release_array_elements = $jni_release_array_elements:ident,
        jni_new_array = $jni_new_array:ident,
        jni_set_array_region = $jni_set_array_region:ident]),*) => {
        $(
            #[allow(dead_code)]
            struct $rust_arr_wrapper {
                array: $jni_arr_type,
                data: *mut $jni_elem_type,
                env: *mut JNIEnv,
            }
            #[allow(dead_code)]
            impl $rust_arr_wrapper {
                fn new(env: *mut JNIEnv, array: $jni_arr_type) -> $rust_arr_wrapper {
                    assert!(!array.is_null());
                    let data =
                        unsafe { (**env).$jni_get_array_elements.unwrap()(env, array,
                                                                          ::std::ptr::null_mut()) };
                    $rust_arr_wrapper { array, data, env }
                }
                fn to_slice(&self) -> &[$rust_elem_type] {
                    unsafe {
                        let len: jsize = (**self.env).GetArrayLength.unwrap()(self.env, self.array);
                        assert!((len as u64) <= (usize::max_value() as u64));
                        ::std::slice::from_raw_parts(self.data, len as usize)
                    }
                }
                fn from_slice_to_raw(arr: &[$rust_elem_type], env: *mut JNIEnv) -> $jni_arr_type {
                    assert!((arr.len() as u64) <= (jsize::max_value() as u64));
                    let jarr: $jni_arr_type = unsafe {
                        (**env).$jni_new_array.unwrap()(env, arr.len() as jsize)
                    };
                    assert!(!jarr.is_null());
                    unsafe {
                        (**env).$jni_set_array_region.unwrap()(env, jarr, 0,
                                                               arr.len() as jsize, arr.as_ptr());
                        if (**env).ExceptionCheck.unwrap()(env) != 0 {
                            panic!("{}:{} {} failed", file!(), line!(),
                                   stringify!($jni_set_array_region));
                        }
                    }
                    jarr
                }
            }

            #[allow(dead_code)]
            impl Drop for $rust_arr_wrapper {
                fn drop(&mut self) {
                    assert!(!self.env.is_null());
                    assert!(!self.array.is_null());
                    unsafe {
                        (**self.env).$jni_release_array_elements.unwrap()(
                            self.env,
                            self.array,
                            self.data,
                            JNI_ABORT as jint,
                        )
                    };
                }
            }
        )*
    }
}

define_array_handling_code!(
    [
        jni_arr_type = jbyteArray,
        rust_arr_wrapper = JavaByteArray,
        jni_get_array_elements = GetByteArrayElements,
        jni_elem_type = jbyte,
        rust_elem_type = i8,
        jni_release_array_elements = ReleaseByteArrayElements,
        jni_new_array = NewByteArray,
        jni_set_array_region = SetByteArrayRegion
    ],
    [
        jni_arr_type = jshortArray,
        rust_arr_wrapper = JavaShortArray,
        jni_get_array_elements = GetShortArrayElements,
        jni_elem_type = jshort,
        rust_elem_type = i16,
        jni_release_array_elements = ReleaseShortArrayElements,
        jni_new_array = NewShortArray,
        jni_set_array_region = SetShortArrayRegion
    ],
    [
        jni_arr_type = jintArray,
        rust_arr_wrapper = JavaIntArray,
        jni_get_array_elements = GetIntArrayElements,
        jni_elem_type = jint,
        rust_elem_type = i32,
        jni_release_array_elements = ReleaseIntArrayElements,
        jni_new_array = NewIntArray,
        jni_set_array_region = SetIntArrayRegion
    ],
    [
        jni_arr_type = jlongArray,
        rust_arr_wrapper = JavaLongArray,
        jni_get_array_elements = GetLongArrayElements,
        jni_elem_type = jlong,
        rust_elem_type = i64,
        jni_release_array_elements = ReleaseLongArrayElements,
        jni_new_array = NewLongArray,
        jni_set_array_region = SetLongArrayRegion
    ],
    [
        jni_arr_type = jfloatArray,
        rust_arr_wrapper = JavaFloatArray,
        jni_get_array_elements = GetFloatArrayElements,
        jni_elem_type = jfloat,
        rust_elem_type = f32,
        jni_release_array_elements = ReleaseFloatArrayElements,
        jni_new_array = NewFloatArray,
        jni_set_array_region = SetFloatArrayRegion
    ],
    [
        jni_arr_type = jdoubleArray,
        rust_arr_wrapper = JavaDoubleArray,
        jni_get_array_elements = GetDoubleArrayElements,
        jni_elem_type = jdouble,
        rust_elem_type = f64,
        jni_release_array_elements = ReleaseDoubleArrayElements,
        jni_new_array = NewDoubleArray,
        jni_set_array_region = SetDoubleArrayRegion
    ]
);

foreign_typemap!(
    ($p:r_type) <T> Vec<T> => &[T] {
        $out = $p.as_slice();
    };
);

foreign_typemap!(
    ($p:r_type) &[i32] => jintArray {
        $out = JavaIntArray::from_slice_to_raw($p, env);
    };
    (f_type) => "int []";
);

foreign_typemap!(
    ($p:r_type) JavaIntArray <= jintArray {
        $out = JavaIntArray::new(env, $p);
    };
    (f_type) <= "int []";
);
foreign_typemap!(
    ($p:r_type) &[i32] <= JavaIntArray {
        $out = $p.to_slice();
    };
);

foreign_typemap!(
    ($p:r_type) &[i64] => jlongArray {
        $out = JavaLongArray::from_slice_to_raw($p, env);
    };
    (f_type) => "long []";
);

foreign_typemap!(
    ($p:r_type) JavaLongArray <= jlongArray {
        $out = JavaLongArray::new(env, $p);
    };
    (f_type) <= "long []";
);
foreign_typemap!(
    ($p:r_type) &[i64] <= JavaLongArray {
        $out = $p.to_slice();
    };
);

foreign_typemap!(
    ($p:r_type) &[f32] => jfloatArray {
        $out = JavaFloatArray::from_slice_to_raw($p, env);
    };
    (f_type) => "float []";
);

foreign_typemap!(
    ($p:r_type) JavaFloatArray <= jfloatArray {
        $out = JavaFloatArray::new(env, $p);
    };
    (f_type) <= "float []";
);
foreign_typemap!(
    ($p:r_type) &[f32] <= JavaFloatArray {
        $out = $p.to_slice();
    };
);

foreign_typemap!(
    ($p:r_type) &[f64] => jdoubleArray {
        $out = JavaDoubleArray::from_slice_to_raw($p, env);
    };
    (f_type) => "double []";
);

foreign_typemap!(
    ($p:r_type) JavaDoubleArray <= jdoubleArray {
        $out = JavaDoubleArray::new(env, $p);
    };
    (f_type) <= "double []";
);
foreign_typemap!(
    ($p:r_type) &[f64] <= JavaDoubleArray {
        $out = $p.to_slice();
    };
);

foreign_typemap!(
    ($p:r_type) &[i8] => jbyteArray {
        $out = JavaByteArray::from_slice_to_raw($p, env);
    };
    (f_type) => "byte []";
);

foreign_typemap!(
    ($p:r_type) JavaByteArray <= jbyteArray {
        $out = JavaByteArray::new(env, $p);
    };
    (f_type) <= "byte []";
);
foreign_typemap!(
    ($p:r_type) &[i8] <= JavaByteArray {
        $out = $p.to_slice();
    };
);

foreign_typemap!(
    ($p:r_type) &[i16] => jshortArray {
        $out = JavaShortArray::from_slice_to_raw($p, env);
    };
    (f_type) => "short []";
);

foreign_typemap!(
    ($p:r_type) JavaShortArray <= jshortArray {
        $out = JavaShortArray::new(env, $p);
    };
    (f_type) <= "short []";
);
foreign_typemap!(
    ($p:r_type) &[i16] <= JavaShortArray {
        $out = $p.to_slice();
    };
);

foreign_typemap!(
    ($p:r_type) String => &str {
        $out = $p.as_str();
    };
);

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

#[cfg(target_pointer_width = "32")]
impl SwigFrom<isize> for jint {
    fn swig_from(x: isize, _: *mut JNIEnv) -> Self {
        x as jint
    }
}

#[cfg(target_pointer_width = "64")]
impl SwigFrom<isize> for jlong {
    fn swig_from(x: isize, _: *mut JNIEnv) -> Self {
        x as jlong
    }
}

#[cfg(target_pointer_width = "32")]
impl SwigFrom<usize> for jlong {
    fn swig_from(x: usize, _: *mut JNIEnv) -> Self {
        x as jlong
    }
}

#[cfg(target_pointer_width = "64")]
impl SwigFrom<usize> for jlong {
    fn swig_from(x: usize, _: *mut JNIEnv) -> Self {
        let x = x as u64;
        u64_to_jlong_checked(x)
    }
}

foreign_typemap!(
    ($p:r_type) &str => String {
        $out = $p.to_string();
    };
);

#[allow(dead_code)]
fn to_java_util_optional_double(
    env: *mut JNIEnv,
    x: Option<f64>,
) -> internal_aliases::JOptionalDouble {
    let class: jclass = swig_jni_find_class!(JAVA_UTIL_OPTIONAL_DOUBLE, "java/util/OptionalDouble");
    assert!(!class.is_null(),);

    match x {
        Some(val) => {
            let of_m: jmethodID = swig_jni_get_static_method_id!(
                JAVA_UTIL_OPTIONAL_DOUBLE_OF,
                JAVA_UTIL_OPTIONAL_DOUBLE,
                "of",
                "(D)Ljava/util/OptionalDouble;"
            );
            assert!(!of_m.is_null());

            let ret = unsafe {
                let ret = (**env).CallStaticObjectMethod.unwrap()(env, class, of_m, val);
                if (**env).ExceptionCheck.unwrap()(env) != 0 {
                    panic!("OptionalDouble.of failed: catch exception");
                }
                ret
            };

            assert!(!ret.is_null());
            ret
        }
        None => {
            let empty_m: jmethodID = swig_jni_get_static_method_id!(
                JAVA_UTIL_OPTIONAL_DOUBLE_EMPTY,
                JAVA_UTIL_OPTIONAL_DOUBLE,
                "empty",
                "()Ljava/util/OptionalDouble;"
            );
            assert!(!empty_m.is_null());

            let ret = unsafe {
                let ret = (**env).CallStaticObjectMethod.unwrap()(env, class, empty_m);
                if (**env).ExceptionCheck.unwrap()(env) != 0 {
                    panic!("OptionalDouble.empty failed: catch exception");
                }
                ret
            };
            assert!(!ret.is_null());
            ret
        }
    }
}

#[allow(dead_code)]
fn from_java_lang_double_to_rust(env: *mut JNIEnv, x: internal_aliases::JDouble) -> Option<f64> {
    if x.is_null() {
        None
    } else {
        let x = unsafe { (**env).NewLocalRef.unwrap()(env, x) };
        if x.is_null() {
            None
        } else {
            let class: jclass = swig_jni_find_class!(JAVA_LANG_DOUBLE, "java/lang/Double");
            assert!(!class.is_null());
            let double_value_m: jmethodID = swig_jni_get_method_id!(
                JAVA_LANG_DOUBLE_DOUBLE_VALUE_METHOD,
                JAVA_LANG_DOUBLE,
                "doubleValue",
                "()D",
            );
            assert!(!double_value_m.is_null(),);
            let ret: f64 = unsafe {
                let ret = (**env).CallDoubleMethod.unwrap()(env, x, double_value_m);
                if (**env).ExceptionCheck.unwrap()(env) != 0 {
                    panic!("Double.doubleValue failed: catch exception");
                }
                (**env).DeleteLocalRef.unwrap()(env, x);
                ret
            };
            Some(ret)
        }
    }
}

foreign_typemap!(
    ($p:r_type) Option<f64> <= internal_aliases::JDouble {
        $out = from_java_lang_double_to_rust(env, $p);
    };
    (f_type, option = "NoNullAnnotations") <= "Double";
    (f_type, option = "NullAnnotations") <= "@Nullable Double";
);

foreign_typemap!(
    ($p:r_type) Option<f64> => internal_aliases::JOptionalDouble {
        $out = to_java_util_optional_double(env, $p);
    };
    (f_type, option = "NoNullAnnotations") => "java.util.OptionalDouble";
    (f_type, option = "NullAnnotations") => "@NonNull java.util.OptionalDouble";
);

#[allow(dead_code)]
fn from_java_lang_float_to_rust(env: *mut JNIEnv, x: internal_aliases::JFloat) -> Option<f32> {
    if x.is_null() {
        None
    } else {
        let x = unsafe { (**env).NewLocalRef.unwrap()(env, x) };
        if x.is_null() {
            None
        } else {
            let class: jclass = swig_jni_find_class!(JAVA_LANG_FLOAT, "java/lang/Float");
            assert!(!class.is_null());
            let float_value_m: jmethodID = swig_jni_get_method_id!(
                JAVA_LANG_FLOAT_FLOAT_VALUE,
                JAVA_LANG_FLOAT,
                "floatValue",
                "()F"
            );
            assert!(!float_value_m.is_null());

            let ret: f32 = unsafe {
                let ret = (**env).CallFloatMethod.unwrap()(env, x, float_value_m);
                if (**env).ExceptionCheck.unwrap()(env) != 0 {
                    panic!("Float.floatValue failed: catch exception");
                }
                (**env).DeleteLocalRef.unwrap()(env, x);
                ret
            };
            Some(ret)
        }
    }
}

foreign_typemap!(
    ($p:r_type) Option<f32> <= internal_aliases::JFloat {
        $out = from_java_lang_float_to_rust(env, $p);
    };
    (f_type, option = "NoNullAnnotations") <= "Float";
    (f_type, option = "NullAnnotations") <= "@Nullable Float";
);

foreign_typemap!(
    ($p:r_type) Option<f32> => internal_aliases::JOptionalDouble {
        $out = to_java_util_optional_double(env, $p.map(f64::from));
    };
);

#[allow(dead_code)]
fn to_java_util_optional_long(env: *mut JNIEnv, x: Option<i64>) -> internal_aliases::JOptionalLong {
    let class: jclass = swig_jni_find_class!(JAVA_UTIL_OPTIONAL_LONG, "java/util/OptionalLong");
    assert!(!class.is_null(),);
    match x {
        Some(val) => {
            let of_m: jmethodID = swig_jni_get_static_method_id!(
                JAVA_UTIL_OPTIONAL_LONG_OF,
                JAVA_UTIL_OPTIONAL_LONG,
                "of",
                "(J)Ljava/util/OptionalLong;"
            );
            assert!(!of_m.is_null());

            let ret = unsafe {
                let ret = (**env).CallStaticObjectMethod.unwrap()(env, class, of_m, val);
                if (**env).ExceptionCheck.unwrap()(env) != 0 {
                    panic!("OptionalLong.of failed: catch exception");
                }
                ret
            };

            assert!(!ret.is_null());
            ret
        }
        None => {
            let empty_m: jmethodID = swig_jni_get_static_method_id!(
                JAVA_UTIL_OPTIONAL_LONG_EMPTY,
                JAVA_UTIL_OPTIONAL_LONG,
                "empty",
                "()Ljava/util/OptionalLong;",
            );
            assert!(!empty_m.is_null());

            let ret = unsafe {
                let ret = (**env).CallStaticObjectMethod.unwrap()(env, class, empty_m);
                if (**env).ExceptionCheck.unwrap()(env) != 0 {
                    panic!("OptionalLong.empty failed: catch exception");
                }
                ret
            };
            assert!(!ret.is_null());
            ret
        }
    }
}

#[allow(dead_code)]
fn from_java_lang_long_to_rust(env: *mut JNIEnv, x: internal_aliases::JLong) -> Option<i64> {
    if x.is_null() {
        None
    } else {
        let x = unsafe { (**env).NewLocalRef.unwrap()(env, x) };
        if x.is_null() {
            None
        } else {
            let class: jclass = swig_jni_find_class!(JAVA_LANG_LONG, "java/lang/Long");
            assert!(!class.is_null());
            let long_value_m: jmethodID = swig_jni_get_method_id!(
                JAVA_LANG_LONG_LONG_VALUE,
                JAVA_LANG_LONG,
                "longValue",
                "()J"
            );
            assert!(!long_value_m.is_null());

            let ret: i64 = unsafe {
                let ret = (**env).CallLongMethod.unwrap()(env, x, long_value_m);
                if (**env).ExceptionCheck.unwrap()(env) != 0 {
                    panic!("Long.longValue failed: catch exception");
                }
                (**env).DeleteLocalRef.unwrap()(env, x);
                ret
            };
            Some(ret)
        }
    }
}

foreign_typemap!(
    ($p:r_type) Option<i64> <= internal_aliases::JLong {
        $out = from_java_lang_long_to_rust(env, $p);
    };
    (f_type, option = "NoNullAnnotations") <= "Long";
    (f_type, option = "NullAnnotations") <= "@Nullable Long";
);

foreign_typemap!(
    ($p:r_type) Option<i64> => internal_aliases::JOptionalLong {
        $out = to_java_util_optional_long(env, $p);
    };
    (f_type, option = "NoNullAnnotations") => "java.util.OptionalLong";
    (f_type, option = "NullAnnotations") => "@NonNull java.util.OptionalLong";
);

#[allow(dead_code)]
fn from_java_lang_int_to_rust(env: *mut JNIEnv, x: internal_aliases::JInteger) -> Option<i32> {
    if x.is_null() {
        None
    } else {
        let x = unsafe { (**env).NewLocalRef.unwrap()(env, x) };
        if x.is_null() {
            None
        } else {
            let class: jclass = swig_jni_find_class!(JAVA_LANG_INTEGER, "java/lang/Integer");
            assert!(!class.is_null());
            let int_value_m: jmethodID = swig_jni_get_method_id!(
                JAVA_LANG_INTEGER_INT_VALUE,
                JAVA_LANG_INTEGER,
                "intValue",
                "()I"
            );
            assert!(!int_value_m.is_null(),);

            let ret: i32 = unsafe {
                let ret = (**env).CallIntMethod.unwrap()(env, x, int_value_m);
                if (**env).ExceptionCheck.unwrap()(env) != 0 {
                    panic!("Integer.intValue failed: catch exception");
                }
                (**env).DeleteLocalRef.unwrap()(env, x);
                ret
            };
            Some(ret)
        }
    }
}

#[allow(dead_code)]
fn from_java_lang_byte_to_rust(env: *mut JNIEnv, x: internal_aliases::JByte) -> Option<i8> {
    if x.is_null() {
        None
    } else {
        let x = unsafe { (**env).NewLocalRef.unwrap()(env, x) };
        if x.is_null() {
            None
        } else {
            let class: jclass = swig_jni_find_class!(JAVA_LANG_BYTE, "java/lang/Byte");
            assert!(!class.is_null());
            let byte_value_m: jmethodID = swig_jni_get_method_id!(
                JAVA_LANG_BYTE_BYTE_VALUE,
                JAVA_LANG_BYTE,
                "byteValue",
                "()B"
            );
            assert!(!byte_value_m.is_null(),);

            let ret: i8 = unsafe {
                let ret = (**env).CallByteMethod.unwrap()(env, x, byte_value_m);
                if (**env).ExceptionCheck.unwrap()(env) != 0 {
                    panic!("Byte.byteValue failed: catch exception");
                }
                (**env).DeleteLocalRef.unwrap()(env, x);
                ret
            };
            Some(ret)
        }
    }
}

#[allow(dead_code)]
fn from_java_lang_short_to_rust(env: *mut JNIEnv, x: internal_aliases::JByte) -> Option<i16> {
    if x.is_null() {
        None
    } else {
        let x = unsafe { (**env).NewLocalRef.unwrap()(env, x) };
        if x.is_null() {
            None
        } else {
            let class: jclass = swig_jni_find_class!(JAVA_LANG_SHORT, "java/lang/Short");
            assert!(!class.is_null());
            let short_value_m: jmethodID = swig_jni_get_method_id!(
                JAVA_LANG_SHORT_SHORT_VALUE,
                JAVA_LANG_SHORT,
                "shortValue",
                "()S"
            );
            assert!(!short_value_m.is_null());

            let ret: i16 = unsafe {
                let ret = (**env).CallShortMethod.unwrap()(env, x, short_value_m);
                if (**env).ExceptionCheck.unwrap()(env) != 0 {
                    panic!("Short.shortValue failed: catch exception");
                }
                (**env).DeleteLocalRef.unwrap()(env, x);
                ret
            };
            Some(ret)
        }
    }
}

foreign_typemap!(
    ($p:r_type) Option<i32> <= internal_aliases::JInteger {
        $out = from_java_lang_int_to_rust(env, $p);
    };
    (f_type, option = "NoNullAnnotations") <= "Integer";
    (f_type, option = "NullAnnotations") <= "@Nullable Integer";
);

#[allow(dead_code)]
fn to_java_util_optional_int(env: *mut JNIEnv, x: Option<i32>) -> jobject {
    let class: jclass = swig_jni_find_class!(JAVA_UTIL_OPTIONAL_INT, "java/util/OptionalInt");
    assert!(!class.is_null(),);
    match x {
        Some(val) => {
            let of_m: jmethodID = swig_jni_get_static_method_id!(
                JAVA_UTIL_OPTIONAL_INT_OF,
                JAVA_UTIL_OPTIONAL_INT,
                "of",
                "(I)Ljava/util/OptionalInt;"
            );
            assert!(!of_m.is_null());

            let ret = unsafe {
                let ret = (**env).CallStaticObjectMethod.unwrap()(env, class, of_m, val);
                if (**env).ExceptionCheck.unwrap()(env) != 0 {
                    panic!("OptionalInt.of failed: catch exception");
                }
                ret
            };

            assert!(!ret.is_null());
            ret
        }
        None => {
            let empty_m: jmethodID = swig_jni_get_static_method_id!(
                JAVA_UTIL_OPTIONAL_INT_EMPTY,
                JAVA_UTIL_OPTIONAL_INT,
                "empty",
                "()Ljava/util/OptionalInt;"
            );
            assert!(!empty_m.is_null());

            let ret = unsafe {
                let ret = (**env).CallStaticObjectMethod.unwrap()(env, class, empty_m);
                if (**env).ExceptionCheck.unwrap()(env) != 0 {
                    panic!("OptionalInt.empty failed: catch exception");
                }
                ret
            };
            assert!(!ret.is_null());
            ret
        }
    }
}

foreign_typemap!(
    ($p:r_type) Option<i32> => internal_aliases::JOptionalInt {
        $out = to_java_util_optional_int(env, $p);
    };
    (f_type, option = "NoNullAnnotations") => "java.util.OptionalInt";
    (f_type, option = "NullAnnotations") => "@NonNull java.util.OptionalInt";
);

foreign_typemap!(
    ($p:r_type) Option<i8> <= internal_aliases::JByte {
        $out = from_java_lang_byte_to_rust(env, $p);
    };
    (f_type, option = "NoNullAnnotations") <= "Byte";
    (f_type, option = "NullAnnotations") <= "@Nullable Byte";
);

foreign_typemap!(
    ($p:r_type) Option<i8> => internal_aliases::JOptionalInt {
        $out = to_java_util_optional_int(env, $p.map(i32::from));
    };
);

foreign_typemap!(
    ($p:r_type) Option<i16> <= internal_aliases::JShort {
        $out = from_java_lang_short_to_rust(env, $p);
    };
    (f_type, option = "NoNullAnnotations") <= "Short";
    (f_type, option = "NullAnnotations") <= "@Nullable Short";
);

foreign_typemap!(
    ($p:r_type) Option<i16> => internal_aliases::JOptionalInt {
        $out = to_java_util_optional_int(env, $p.map(i32::from));
    };
);

foreign_typemap!(
    ($p:r_type) <T: SwigForeignClass> Option<T> => jlong {
        $out = match $p {
            Some(x) => {
                let ptr = <swig_subst_type!(T)>::box_object(x);
                debug_assert_ne!(0, ptr);
                ptr
            }
            None => 0,
        };
    };
    ($p:f_type, option = "NoNullAnnotations") => "java.util.Optional<swig_f_type!(T)>" r#"
        $out;
        if ($p != 0) {
            $out = java.util.Optional.of(new swig_f_type!(T)(InternalPointerMarker.RAW_PTR, $p));
        } else {
            $out = java.util.Optional.empty();
        }
"#;
    ($p:f_type, option = "NullAnnotations") => "@NonNull java.util.Optional<swig_f_type!(T, NoNullAnnotations)>" r#"
        $out;
        if ($p != 0) {
            $out = java.util.Optional.of(new swig_f_type!(T, NoNullAnnotations)(InternalPointerMarker.RAW_PTR, $p));
        } else {
            $out = java.util.Optional.empty();
        }
"#;
);

foreign_typemap!(
    ($p:r_type) <T: SwigForeignClass> Option<T> <= jlong {
        $out = if $p != 0{
            let o: swig_subst_type!(T) = <swig_subst_type!(T)>::unbox_object($p);
            Some(o)
        } else {
            None
        };
    };
    ($p:f_type, option = "NoNullAnnotations", unique_prefix = "/*opt*/") <= "/*opt*/swig_f_type!(T)" r#"
        $out = 0;//TODO: use ptr::null() for corresponding constant
        if ($p != null) {
            $out = $p.mNativeObj;
            $p.mNativeObj = 0;
        }
"#;
    ($p:f_type, option = "NullAnnotations", unique_prefix = "/*opt*/") <= "/*opt*/@Nullable swig_f_type!(T, NoNullAnnotations)" r#"
        $out = 0;//TODO: use ptr::null() for corresponding constant
        if ($p != null) {
            $out = $p.mNativeObj;
            $p.mNativeObj = 0;
        }
"#;
);

foreign_typemap!(
    ($p:r_type) <T: SwigForeignClass> Option<&T> <= jlong {
        let obj_ptr;
        $out = if $p != 0{
            obj_ptr = <swig_subst_type!(T)>::to_pointer($p);
            let o: &swig_subst_type!(T) = unsafe { obj_ptr.as_ref() };
            Some(o)
        } else {
            None
        };
    };
    ($p:f_type, option = "NoNullAnnotations", unique_prefix = "/*opt ref*/") <= "/*opt ref*/swig_f_type!(T)" r#"
        $out = 0;//TODO: use ptr::null() for corresponding constant
        if ($p != null) {
            $out = $p.mNativeObj;
        }
"#;
    ($p:f_type, option = "NullAnnotations", unique_prefix = "/*opt ref*/") <= "/*opt ref*/@Nullable swig_f_type!(T, NoNullAnnotations)" r#"
        $out = 0;//TODO: use ptr::null() for corresponding constant
        if ($p != null) {
            $out = $p.mNativeObj;
        }
"#;
);

foreign_typemap!(
    ($p:r_type) Option<String> => internal_aliases::JStringOptStr {
        $out = match $p {
            Some(s) => from_std_string_jstring(s, env),
            None => ::std::ptr::null_mut(),
        };
    };
    ($p:f_type, option = "NoNullAnnotations") => "java.util.Optional<String>" r#"
        $out = java.util.Optional.ofNullable($p);
"#;
    ($p:f_type, option = "NullAnnotations") => "@NonNull java.util.Optional<String>" r#"
        $out = java.util.Optional.ofNullable($p);
"#;
);

foreign_typemap!(
    (r_type) internal_aliases::JStringOptStr;
    (f_type, option = "NoNullAnnotations", unique_prefix = "/*opt*/") "/*opt*/String";
    (f_type, option = "NullAnnotations", unique_prefix = "/*opt*/") "/*opt*/@Nullable String";
);

foreign_typemap!(
    ($p:r_type) Option<&str> <= internal_aliases::JStringOptStr {
        let tmp: JavaString;
        $out = if !$p.is_null() {
            tmp = JavaString::new(env, $p);
            Some(tmp.to_str())
        } else {
            None
        };
    };
);

foreign_typemap!(
    ($p:r_type) <T: SwigForeignCLikeEnum> Option<T> => jint {
        $out = match $p {
            Some(v) => v.as_jint(),
            None => -1,
        };
    };
    ($p:f_type, option = "NoNullAnnotations") => "java.util.Optional<swig_f_type!(T)>" r#"
        $out;
        if ($p != -1) {
            $out = java.util.Optional.of(swig_f_type!(T).fromInt($p));
        } else {
            $out = java.util.Optional.empty();
        }
"#;
    ($p:f_type, option = "NullAnnotations") => "@NonNull java.util.Optional<swig_f_type!(T)>" r#"
        $out;
        if ($p != -1) {
            $out = java.util.Optional.of(swig_f_type!(T).fromInt($p));
        } else {
            $out = java.util.Optional.empty();
        }
"#;
);

foreign_typemap!(
    ($p:r_type) <T: SwigForeignCLikeEnum> Option<T> <= jint {
        $out = if $p != -1 {
            Some(<swig_subst_type!(T)>::from_jint($p))
        } else {
            None
        };
    };
    ($p:f_type, option = "NoNullAnnotations", unique_prefix = "/*opt*/") <= "/*opt*/swig_f_type!(T)" r#"
        $out = ($p != null) ? $p.getValue() : -1;
"#;
    ($p:f_type, option = "NullAnnotations", unique_prefix = "/*opt*/") <= "/*opt*/@Nullable swig_f_type!(T)" r#"
        $out = ($p != null) ? $p.getValue() : -1;
"#;
);

foreign_typemap!(
    ($p:r_type) <T: SwigForeignCLikeEnum> T => jint {
        $out = $p.as_jint();
    };
);

foreign_typemap!(
    ($p:r_type) <T: SwigForeignCLikeEnum> T <= jint {
        $out = <swig_subst_type!(T)>::from_jint($p);
    };
);
