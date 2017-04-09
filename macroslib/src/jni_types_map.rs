#[foreigner_type="boolean"]
impl SwigInto<bool> for jboolean {
    fn swig_into(self, _: *mut JNIEnv) -> bool {
        self != 0
    }
}

#[foreigner_type="boolean"]
impl SwigFrom<bool> for jboolean {
    fn swig_from(x: bool, _: *mut JNIEnv) -> Self {
        if x { 1 as jboolean } else { 0 as jboolean }
    }
}

#[foreigner_type="short/*should be from 0 to 2^8-1*/"]
impl SwigInto<u8> for jshort {
    fn swig_into(self, _: *mut JNIEnv) -> u8 {
        if self < 0 || self > (::std::u8::MAX as jshort) {
            panic!("Expect self from 0 to {}, got {}", ::std::u8::MAX, self);
        }
        self as u8
   }
}

#[foreigner_type="short/*should be from 0 to 2^8-1*/"]
impl SwigFrom<u8> for jshort {
    fn swig_from(x: u8, _: *mut JNIEnv) -> Self {
        x as jshort
   }
}

#[foreigner_type="int/*should be from 0 to 2^16-1*/"]
impl SwigInto<u16> for jint {
   fn swig_into(self, _: *mut JNIEnv) -> u16 {
       if self < 0 || self > (::std::u16::MAX as jint) {
           panic!("Expect self from 0 to {}, got {}", ::std::u16::MAX, self);
       }
       self as u16
   }
}

#[foreigner_type="int/*should be from 0 to 2^16-1*/"]
impl SwigFrom<u16> for jint {
    fn swig_from(x: u16, _: *mut JNIEnv) -> Self {
        x as jint
    }
}

#[foreigner_type="long/*should be from 0 to 2^32-1*/"]
impl SwigInto<u32> for jlong {
   fn swig_into(self, _: *mut JNIEnv) -> u32 {
       if self < 0 || self > (::std::u32::MAX as jlong) {
           panic!("Expect self from 0 to {}, got {}", ::std::u32::MAX, self);
       }
       self as u32
   }
}

#[foreigner_type="long/*should be from 0 to 2^32-1*/"]
impl SwigFrom<u32> for jlong {
    fn swig_from(x: u32, _: *mut JNIEnv) -> Self {
        x as jlong
   }
}

#[foreigner_type="long/*should be >= 0*/"]
impl SwigInto<u64> for jlong {
   fn swig_into(self, _: *mut JNIEnv) -> u64 {
       if self < 0 {
           panic!("Expect self to be positive, got {}", self);
       }
       self as u64
   }
}

#[foreigner_type="long/*should be >= 0*/"]
impl SwigFrom<u64> for jlong {
    fn swig_from(x: u64, _: *mut JNIEnv) -> Self {
        if (::std::i64::MAX as u64) < x {
            error!("u64->jlong type overflow: {}", x);
            ::std::i64::MAX
        } else { x as i64 }
   }
}

#[foreigner_type="byte"]
impl SwigInto<i8> for jbyte {
    fn swig_into(self, _: *mut JNIEnv) -> i8 {
        self
   }
}

#[foreigner_type="byte"]
impl SwigFrom<i8> for jbyte {
    fn swig_from(x: i8, _: *mut JNIEnv) -> Self {
        x
   }
}

#[foreigner_type="short"]
impl SwigInto<i16> for jshort {
    fn swig_into(self, _: *mut JNIEnv) -> i16 {
        self
   }
}

#[foreigner_type="short"]
impl SwigFrom<i16> for jshort {
    fn swig_from(x: i16, _: *mut JNIEnv) -> Self {
        x
   }
}

#[foreigner_type="int"]
impl SwigInto<i32> for jint {
    fn swig_into(self, _: *mut JNIEnv) -> i32 {
        self
   }
}

#[foreigner_type="int"]
impl SwigFrom<i32> for jint {
    fn swig_from(x: i32, _: *mut JNIEnv) -> Self {
        x
   }
}

#[foreigner_type="long"]
impl SwigInto<i64> for jlong {
    fn swig_into(self, _: *mut JNIEnv) -> i64 {
        self
   }
}

#[foreigner_type="long"]
impl SwigFrom<i64> for jlong {
    fn swig_from(x: i64, _: *mut JNIEnv) -> Self {
        x
   }
}

#[foreigner_type="float"]
impl SwigInto<f32> for jfloat {
    fn swig_into(self, _: *mut JNIEnv) -> f32 {
        self
   }
}

#[foreigner_type="float"]
impl SwigFrom<f32> for jfloat {
    fn swig_from(x: f32, _: *mut JNIEnv) -> Self {
        x
   }
}

#[foreigner_type="double"]
impl SwigInto<f64> for jdouble {
    fn swig_into(self, _: *mut JNIEnv) -> f64 {
        self
   }
}

#[foreigner_type="double"]
impl SwigFrom<f64> for jdouble {
    fn swig_from(x: f64, _: *mut JNIEnv) -> Self {
        x
   }
}

#[foreigner_type="String"]
impl SwigInto<JavaString> for jstring {
    fn swig_into(self, env: *mut JNIEnv) -> JavaString {
        JavaString::new(env, self)
    }
}

#[foreigner_type="String"]
impl<'a> SwigFrom<&'a str> for jstring {
    fn swig_from(x: &'a str, env: *mut JNIEnv) -> Self {
        let x = ::std::ffi::CString::new(x).unwrap();
        unsafe { (**env).NewStringUTF.unwrap()(env, x.as_ptr()) }
   }
}

#[foreigner_type="String"]
impl SwigFrom<String> for jstring {
    fn swig_from(x: String, env: *mut JNIEnv) -> Self {
        let x = x.into_bytes();
        let x = unsafe { ::std::ffi::CString::from_vec_unchecked(x) };
        unsafe { (**env).NewStringUTF.unwrap()(env, x.as_ptr()) }
   }
}

impl ::std::ops::Deref for JavaString {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        self.to_str()
    }
}

#[foreigner_type="java.util.Date"]
impl SwigFrom<SystemTime> for jobject {
    fn swig_from(x: SystemTime, env: *mut JNIEnv) -> Self {
        let since_unix_epoch = x.duration_since(::std::time::UNIX_EPOCH).unwrap();
        let mills: jlong = (since_unix_epoch.as_secs() * 1_000 +
                            (since_unix_epoch.subsec_nanos() / 1_000_000) as u64) as jlong;
        let class_name_c = ::std::ffi::CString::new("java/util/Date").unwrap();
        let date_class: jclass = unsafe { (**env).FindClass.unwrap()(env, class_name_c.as_ptr()) };
        assert!(!date_class.is_null());
        let init_name_c = ::std::ffi::CString::new("<init>").unwrap();
        let method_args_c = ::std::ffi::CString::new("(J)V").unwrap();
        let init: jmethodID = unsafe {
            (**env).GetMethodID.unwrap()(env, date_class, init_name_c.as_ptr(),
                                         method_args_c.as_ptr())
        };
        assert!(!init.is_null());
        let x = unsafe { (**env).NewObject.unwrap()(env, date_class, init, mills) };
        assert!(!x.is_null());
        x
   }
}
