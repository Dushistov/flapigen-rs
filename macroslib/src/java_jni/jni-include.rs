mod swig_foreign_types_map {
    #![swig_foreigner_type = "void"]
    #![swig_rust_type = "()"]
    #![swig_foreigner_type = "boolean"]
    #![swig_rust_type = "jboolean"]
    #![swig_foreigner_type = "byte"]
    #![swig_rust_type = "jbyte"]
    #![swig_foreigner_type = "short"]
    #![swig_rust_type = "jshort"]
    #![swig_foreigner_type = "int"]
    #![swig_rust_type = "jint"]
    #![swig_foreigner_type = "long"]
    #![swig_rust_type = "jlong"]
    #![swig_foreigner_type = "String"]
    #![swig_rust_type = "jstring"]
    #![swig_foreigner_type = "float"]
    #![swig_rust_type = "jfloat"]
    #![swig_foreigner_type = "double"]
    #![swig_rust_type = "jdouble"]
    #![swig_foreigner_type = "int []"]
    #![swig_rust_type = "jintArray"]
    #![swig_foreigner_type = "Object"]
    #![swig_rust_type_not_unique = "jobject"]
    #![swig_foreigner_type = "java.util.Date"]
    #![swig_rust_type_not_unique = "jobject"]
    #![swig_foreigner_type = "Object []"]
    #![swig_rust_type_not_unique = "jobjectArray"]
    #![swig_foreigner_type = "java.lang.String []"]
    #![swig_rust_type_not_unique = "jobjectArray"]
}

#[allow(dead_code)]
#[swig_code = "let mut {to_var}: {to_var_type} = {from_var}.swig_into(env);"]
trait SwigInto<T> {
    fn swig_into(self, env: *mut JNIEnv) -> T;
}

#[allow(dead_code)]
#[swig_code = "let mut {to_var}: {to_var_type} = <{to_var_type}>::swig_from({from_var}, env);"]
trait SwigFrom<T> {
    fn swig_from(T, env: *mut JNIEnv) -> Self;
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
trait SwigForeignClass {
    fn jni_class_name() -> *const ::std::os::raw::c_char;
    fn box_object(x: Self) -> jlong;
}

#[allow(unused_macros)]
macro_rules! swig_c_str {
    ($lit:expr) => {
        concat!($lit, "\0").as_ptr()
            as *const ::std::os::raw::c_char
    }
}

#[allow(unused_macros)]
macro_rules! swig_assert_eq_size {
    ($x:ty, $($xs:ty),+ $(,)*) => {
        #[allow(unknown_lints, forget_copy, unused_unsafe, useless_transmute)]
        unsafe {
            use std::mem::{forget, transmute, uninitialized};
            $(forget::<$xs>(transmute(uninitialized::<$x>()));)+
        }
    };
}


#[cfg(target_pointer_width = "32")]
unsafe fn jlong_to_pointer<T>(val: jlong) -> *mut T {
    (val as u32) as *mut T
}

#[cfg(target_pointer_width = "64")]
unsafe fn jlong_to_pointer<T>(val: jlong) -> *mut T {
    val as *mut T
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
struct JavaCallback {
    java_vm: *mut JavaVM,
    this: jobject,
    methods: Vec<jmethodID>,
}

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
                error!("JniEnvHolder: DetachCurrentThread failed: {}", res);
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

        #[cfg(target_os = "android")]
        type GetJNiEnvPtrPtr = *mut *mut JNIEnv;
        #[cfg(not(target_os = "android"))]
        type GetJNiEnvPtrPtr = *mut *mut ::std::os::raw::c_void;

        let res = unsafe {
            (**self.java_vm).GetEnv.unwrap()(
                self.java_vm,
                (&mut env) as *mut *mut JNIEnv as *mut *mut ::std::os::raw::c_void,
                JNI_VERSION_1_6 as jint,
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

        let res = unsafe {
            (**self.java_vm).AttachCurrentThread.unwrap()(
                self.java_vm,
                (&mut env) as *mut *mut JNIEnv as GetJNiEnvPtrPtr,
                ::std::ptr::null_mut(),
            )
        };
        if res != 0 {
            error!(
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
            error!("JavaCallback::drop failed, can not get JNIEnv");
        }
    }
}

#[allow(dead_code)]
fn jni_throw(env: *mut JNIEnv, class_name: *const ::std::os::raw::c_char, message: &str) {

    let ex_class = unsafe { (**env).FindClass.unwrap()(env, class_name) };
    if ex_class.is_null() {
        error!(
            "throw_exception: can not find exp class {:?}, msg {}",
            unsafe { ::std::ffi::CStr::from_ptr(class_name) },
            message
        );
        return;
    }
    let c_message = ::std::ffi::CString::new(message).unwrap();
    let res = unsafe { (**env).ThrowNew.unwrap()(env, ex_class, c_message.as_ptr()) };
    if res != 0 {
        error!("ThrowNew({}) for class {:?} failed", message, unsafe {
            ::std::ffi::CStr::from_ptr(class_name)
        });
    }
}

#[allow(dead_code)]
fn jni_throw_exception(env: *mut JNIEnv, message: &str) {
    jni_throw(env, swig_c_str!("java/lang/Exception"), message)
}

#[swig_to_foreigner_hint = "T"]
impl<T: SwigForeignClass> SwigFrom<T> for jobject {
    fn swig_from(x: T, env: *mut JNIEnv) -> Self {
        object_to_jobject(x, <T>::jni_class_name(), env)
    }
}

#[allow(dead_code)]
fn object_to_jobject<T: SwigForeignClass>(
    obj: T,
    class_id: *const ::std::os::raw::c_char,
    env: *mut JNIEnv,
) -> jobject {
    let jcls: jclass = unsafe { (**env).FindClass.unwrap()(env, class_id) };
    assert!(!jcls.is_null(), "object_to_jobject: FindClass failed");
    let jobj: jobject = unsafe { (**env).AllocObject.unwrap()(env, jcls) };
    assert!(!jobj.is_null(), "object_to_jobject: AllocObject failed");
    let field_id: jfieldID = unsafe {
        (**env).GetFieldID.unwrap()(env, jcls, swig_c_str!("mNativeObj"), swig_c_str!("J"))
    };
    assert!(
        !field_id.is_null(),
        "object_to_jobject: GetFieldID(mNativeObj) failed"
    );
    let ret: jlong = <T>::box_object(obj);
    unsafe {
        (**env).SetLongField.unwrap()(env, jobj, field_id, ret);
        if (**env).ExceptionCheck.unwrap()(env) != 0 {
            panic!("object_to_jobject: Can not set mNativeObj field: catch exception");
        }
    }
    jobj
}

#[swig_to_foreigner_hint = "T []"]
impl<T: SwigForeignClass> SwigFrom<Vec<T>> for jobjectArray {
    fn swig_from(x: Vec<T>, env: *mut JNIEnv) -> Self {
        vec_of_objects_to_jobject_array(x, <T>::jni_class_name(), env)
    }
}


#[allow(dead_code)]
fn vec_of_objects_to_jobject_array<T: SwigForeignClass>(
    mut arr: Vec<T>,
    class_id: *const ::std::os::raw::c_char,
    env: *mut JNIEnv,
) -> jobjectArray {
    let jcls: jclass = unsafe { (**env).FindClass.unwrap()(env, class_id) };
    assert!(!jcls.is_null());
    //TODO: check for arr.len() -> jsize overflow
    let obj_arr: jobjectArray = unsafe {
        (**env).NewObjectArray.unwrap()(env, arr.len() as jsize, jcls, ::std::ptr::null_mut())
    };
    assert!(!obj_arr.is_null());

    let field_id = swig_c_str!("mNativeObj");
    let type_id = swig_c_str!("J");
    let field_id: jfieldID = unsafe { (**env).GetFieldID.unwrap()(env, jcls, field_id, type_id) };
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
    obj_arr
}

#[allow(dead_code)]
trait JniInvalidValue<T> {
    fn invalid_value() -> T;
}

impl<T> JniInvalidValue<*const T> for *const T {
    fn invalid_value() -> *const T {
        ::std::ptr::null()
    }
}

impl<T> JniInvalidValue<*mut T> for *mut T {
    fn invalid_value() -> *mut T {
        ::std::ptr::null_mut()
    }
}

impl JniInvalidValue<()> for () {
    fn invalid_value() {}
}

macro_rules! impl_jni_invalid_value {
    ($($type:ty)*) => ($(
        impl JniInvalidValue<$type> for $type {
            fn invalid_value() -> $type {
                <$type>::default()
            }
        }
    )*)
}

impl_jni_invalid_value! {
    jbyte jshort jint jlong jfloat jdouble
}

#[swig_generic_arg = "T"]
#[swig_generic_arg = "E"]
#[swig_from = "Result<T, E>"]
#[swig_to = "T"]
#[swig_code = "let mut {to_var}:{to_var_type}=jni_unpack_return!({from_var},{function_ret_type}, env);"]
macro_rules! jni_unpack_return {
    ($result_value:expr, $func_ret_type:ty, $env:ident) => {
        {
            let ret = match $result_value {
                Ok(x) => x,
                Err(msg) => {
                    jni_throw_exception($env, &msg);
                    return <$func_ret_type>::invalid_value();
                }
            };
            ret
        }
    }
}

impl SwigInto<bool> for jboolean {
    fn swig_into(self, _: *mut JNIEnv) -> bool {
        self != 0
    }
}

impl SwigFrom<bool> for jboolean {
    fn swig_from(x: bool, _: *mut JNIEnv) -> Self {
        if x {
            1 as jboolean
        } else {
            0 as jboolean
        }
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

#[swig_to_foreigner_hint = "java.util.Date"]
impl SwigFrom<SystemTime> for jobject {
    fn swig_from(x: SystemTime, env: *mut JNIEnv) -> Self {
        let since_unix_epoch = x.duration_since(::std::time::UNIX_EPOCH).unwrap();
        let mills: jlong = (since_unix_epoch.as_secs() * 1_000 +
            (since_unix_epoch.subsec_nanos() / 1_000_000) as u64) as
            jlong;
        let date_class: jclass =
            unsafe { (**env).FindClass.unwrap()(env, swig_c_str!("java/util/Date")) };
        assert!(
            !date_class.is_null(),
            "FindClass for `java/util/Date` failed"
        );
        let init: jmethodID = unsafe {
            (**env).GetMethodID.unwrap()(
                env,
                date_class,
                swig_c_str!("<init>"),
                swig_c_str!("(J)V"),
            )
        };
        assert!(
            !init.is_null(),
            "java/util/Date GetMethodID for init failed"
        );
        let x = unsafe { (**env).NewObject.unwrap()(env, date_class, init, mills) };
        assert!(!x.is_null());
        x
    }
}

impl SwigInto<usize> for i64 {
    fn swig_into(self, _: *mut JNIEnv) -> usize {
        if self < 0 {
            panic!(
                "{}:{} expect self to be positive, got {}",
                file!(),
                line!(),
                self
            );
        } else if (self as u64) > (::std::usize::MAX as u64) {
            panic!("{}:{} too big value for usize {}", file!(), line!(), self);
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
#[swig_to_foreigner_hint = "java.lang.String []"]
impl SwigInto<jobjectArray> for Vec<String> {
    fn swig_into(mut self, env: *mut JNIEnv) -> jobjectArray {
        let class_id = swig_c_str!("java/lang/String");
        let jcls: jclass = unsafe { (**env).FindClass.unwrap()(env, class_id) };
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

#[allow(dead_code)]
struct JavaIntArray {
    array: jintArray,
    data: *mut jint,
    env: *mut JNIEnv,
}

#[allow(dead_code)]
impl JavaIntArray {
    fn new(env: *mut JNIEnv, array: jintArray) -> JavaIntArray {
        assert!(!array.is_null());
        let data =
            unsafe { (**env).GetIntArrayElements.unwrap()(env, array, ::std::ptr::null_mut()) };
        JavaIntArray { array, data, env }
    }
    fn to_slice(&self) -> &[i32] {
        unsafe {
            let len = (**self.env).GetArrayLength.unwrap()(self.env, self.array);
            //TODO: check jsize -> usize conversation safety
            ::std::slice::from_raw_parts(self.data, len as usize)
        }
    }
}

#[allow(dead_code)]
impl Drop for JavaIntArray {
    fn drop(&mut self) {
        assert!(!self.env.is_null());
        assert!(!self.array.is_null());
        unsafe {
            (**self.env).ReleaseIntArrayElements.unwrap()(
                self.env,
                self.array,
                self.data,
                JNI_ABORT as jint,
            )
        };
    }
}

impl SwigDeref for JavaIntArray {
    type Target = [i32];
    fn swig_deref(&self) -> &Self::Target {
        self.to_slice()
    }
}

impl SwigFrom<jintArray> for JavaIntArray {
    fn swig_from(x: jintArray, env: *mut JNIEnv) -> Self {
        JavaIntArray::new(env, x)
    }
}

impl<'a> SwigInto<jintArray> for &'a [i32] {
    fn swig_into(self, env: *mut JNIEnv) -> jintArray {
        //TODO: check conversation usize <-> jsize in this function
        let jarr: jintArray = unsafe { (**env).NewIntArray.unwrap()(env, self.len() as jsize) };
        if jarr.is_null() {
            panic!("Can not create jintArray");
        }
        unsafe {
            (**env).SetIntArrayRegion.unwrap()(env, jarr, 0, self.len() as jsize, self.as_ptr());
            if (**env).ExceptionCheck.unwrap()(env) != 0 {
                panic!("{}:{} SetIntArrayRegion failed", file!(), line!());
            }
        }
        jarr
    }
}

impl SwigDeref for String {
    type Target = str;
    fn swig_deref(&self) -> &str {
        self
    }
}

impl<T> SwigDeref for Arc<Mutex<T>> {
    type Target = Mutex<T>;
    fn swig_deref(&self) -> &Mutex<T> {
        self
    }
}


impl<'a, T> SwigFrom<&'a Mutex<T>> for MutexGuard<'a, T> {
    fn swig_from(m: &'a Mutex<T>, _: *mut JNIEnv) -> MutexGuard<'a, T> {
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
    fn swig_from(m: &'a RefCell<T>, _: *mut JNIEnv) -> Ref<'a, T> {
        m.borrow()
    }
}

impl<'a, T> SwigFrom<&'a RefCell<T>> for RefMut<'a, T> {
    fn swig_from(m: &'a RefCell<T>, _: *mut JNIEnv) -> RefMut<'a, T> {
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
    fn swig_from(x: usize, env: *mut JNIEnv) -> Self {
        let x = x as u64;
        <jlong>::swig_from(x, env)
    }
}
