use std::{cell::Cell, ptr};

use super::*;

struct MockString {
    chars: Vec<jchar>,
    active_leases: Cell<usize>,
}

struct MockIntArray {
    elements: Vec<jint>,
    active_leases: Cell<usize>,
}

fn mock_string(value: &str) -> jstring {
    Box::into_raw(Box::new(MockString {
        chars: value.encode_utf16().collect(),
        active_leases: Cell::new(0),
    }))
    .cast()
}

// SAFETY: `string` must have been allocated by `mock_string` or `new_string`,
// and this function must be called exactly once after the last JNI use.
unsafe fn take_string(string: jstring) -> String {
    let string = Box::from_raw(string.cast::<MockString>());
    assert_eq!(string.active_leases.get(), 0);
    String::from_utf16(&string.chars).unwrap()
}

fn mock_int_array(elements: Vec<jint>) -> jintArray {
    Box::into_raw(Box::new(MockIntArray {
        elements,
        active_leases: Cell::new(0),
    }))
    .cast()
}

// SAFETY: `array` must have been allocated by `mock_int_array` or
// `new_int_array`, and this function must be called exactly once after JNI use.
unsafe fn take_int_array(array: jintArray) -> Vec<jint> {
    let array = Box::from_raw(array.cast::<MockIntArray>());
    assert_eq!(array.active_leases.get(), 0);
    array.elements
}

unsafe extern "system" fn get_string_length(_: *mut JNIEnv, string: jstring) -> jsize {
    let string = &*string.cast::<MockString>();
    string.chars.len().try_into().unwrap()
}

unsafe extern "system" fn get_string_chars(
    _: *mut JNIEnv,
    string: jstring,
    is_copy: *mut jboolean,
) -> *const jchar {
    let string = &*string.cast::<MockString>();
    string.active_leases.set(string.active_leases.get() + 1);
    if !is_copy.is_null() {
        *is_copy = JNI_FALSE;
    }
    string.chars.as_ptr()
}

unsafe extern "system" fn release_string_chars(
    _: *mut JNIEnv,
    string: jstring,
    chars: *const jchar,
) {
    let string = &*string.cast::<MockString>();
    assert_eq!(chars, string.chars.as_ptr());
    assert_eq!(string.active_leases.get(), 1);
    string.active_leases.set(0);
}

unsafe extern "system" fn new_string(_: *mut JNIEnv, chars: *const jchar, len: jsize) -> jstring {
    assert!(len >= 0);
    assert!(!chars.is_null());
    let chars = std::slice::from_raw_parts(chars, len as usize);
    Box::into_raw(Box::new(MockString {
        chars: chars.to_vec(),
        active_leases: Cell::new(0),
    }))
    .cast()
}

unsafe extern "system" fn get_array_length(_: *mut JNIEnv, array: jarray) -> jsize {
    let array = &*array.cast::<MockIntArray>();
    array.elements.len().try_into().unwrap()
}

unsafe extern "system" fn new_int_array(_: *mut JNIEnv, len: jsize) -> jintArray {
    assert!(len >= 0);
    mock_int_array(vec![0; len as usize])
}

unsafe extern "system" fn get_int_array_elements(
    _: *mut JNIEnv,
    array: jintArray,
    is_copy: *mut jboolean,
) -> *mut jint {
    let array = &mut *array.cast::<MockIntArray>();
    assert_eq!(array.active_leases.get(), 0);
    array.active_leases.set(1);
    if !is_copy.is_null() {
        *is_copy = JNI_FALSE;
    }
    array.elements.as_mut_ptr()
}

unsafe extern "system" fn release_int_array_elements(
    _: *mut JNIEnv,
    array: jintArray,
    elements: *mut jint,
    mode: jint,
) {
    let array = &*array.cast::<MockIntArray>();
    assert_eq!(elements, array.elements.as_ptr().cast_mut());
    assert_eq!(mode, JNI_ABORT);
    assert_eq!(array.active_leases.get(), 1);
    array.active_leases.set(0);
}

unsafe extern "system" fn set_int_array_region(
    _: *mut JNIEnv,
    array: jintArray,
    start: jsize,
    len: jsize,
    source: *const jint,
) {
    assert!(start >= 0 && len >= 0);
    let array = &mut *array.cast::<MockIntArray>();
    assert_eq!(array.active_leases.get(), 0);
    let start = start as usize;
    let end = start + len as usize;
    array.elements[start..end].copy_from_slice(std::slice::from_raw_parts(source, len as usize));
}

unsafe extern "system" fn exception_check(_: *mut JNIEnv) -> jboolean {
    JNI_FALSE
}

struct MockEnv {
    _table: Box<JNINativeInterface_>,
    env: Box<JNIEnv>,
}

impl MockEnv {
    fn new() -> Self {
        // SAFETY: jni-sys 0.3 represents every function-table entry as a
        // nullable function pointer, and the four reserved fields are pointers.
        let mut table: Box<JNINativeInterface_> = Box::new(unsafe { std::mem::zeroed() });
        table.GetStringLength = Some(get_string_length);
        table.GetStringChars = Some(get_string_chars);
        table.ReleaseStringChars = Some(release_string_chars);
        table.NewString = Some(new_string);
        table.GetArrayLength = Some(get_array_length);
        table.NewIntArray = Some(new_int_array);
        table.GetIntArrayElements = Some(get_int_array_elements);
        table.ReleaseIntArrayElements = Some(release_int_array_elements);
        table.SetIntArrayRegion = Some(set_int_array_region);
        table.ExceptionCheck = Some(exception_check);
        let env = Box::new(ptr::null());
        Self { _table: table, env }
    }

    fn as_ptr(&mut self) -> *mut JNIEnv {
        *self.env = &*self._table as *const JNINativeInterface_;
        &mut *self.env
    }
}

#[test]
fn object_handles_are_destroyed_once() {
    let mut env = MockEnv::new();
    let env = env.as_ptr();
    let class = ptr::null_mut();

    assert_eq!(BOX_DROPS.load(Ordering::SeqCst), 0);
    let boxed = Java_miri_1tests_BoxOwned_init(env, class, 11);
    assert_eq!(Java_miri_1tests_BoxOwned_do_1value(env, class, boxed), 11);
    Java_miri_1tests_BoxOwned_do_1delete(env, class, boxed);
    assert_eq!(BOX_DROPS.load(Ordering::SeqCst), 1);

    assert_eq!(RC_DROPS.load(Ordering::SeqCst), 0);
    let rc = Java_miri_1tests_RcOwned_init(env, class, 22);
    assert_eq!(Java_miri_1tests_RcOwned_do_1value(env, class, rc), 22);
    Java_miri_1tests_RcOwned_do_1delete(env, class, rc);
    assert_eq!(RC_DROPS.load(Ordering::SeqCst), 1);

    assert_eq!(ARC_DROPS.load(Ordering::SeqCst), 0);
    let arc = Java_miri_1tests_ArcOwned_init(env, class, 33);
    assert_eq!(Java_miri_1tests_ArcOwned_do_1value(env, class, arc), 33);
    Java_miri_1tests_ArcOwned_do_1delete(env, class, arc);
    assert_eq!(ARC_DROPS.load(Ordering::SeqCst), 1);
}

#[test]
fn utf16_strings_round_trip_and_release_chars() {
    let mut env = MockEnv::new();
    let env = env.as_ptr();
    let class = ptr::null_mut();

    for value in ["", "hello", "A\0🦀"] {
        let input = mock_string(value);
        let output = Java_miri_1tests_Text_echo(env, class, input);
        // SAFETY: each pointer belongs to this test, and the call has returned.
        unsafe {
            assert_eq!(take_string(input), value);
            assert_eq!(take_string(output), value);
        }

        let input = mock_string(value);
        let output = Java_miri_1tests_Text_decorate(env, class, input);
        // SAFETY: each pointer belongs to this test, and the call has returned.
        unsafe {
            assert_eq!(take_string(input), value);
            assert_eq!(take_string(output), format!("<{value}>"));
        }
    }
}

#[test]
fn primitive_arrays_round_trip_and_release_elements() {
    let mut env = MockEnv::new();
    let env = env.as_ptr();
    let class = ptr::null_mut();

    for values in [vec![], vec![7], vec![2, 3, 5, 8]] {
        let input = mock_int_array(values.clone());
        assert_eq!(
            Java_miri_1tests_Numbers_sum(env, class, input),
            values.iter().sum::<i32>()
        );
        // SAFETY: the JNI call has returned and released its element lease.
        assert_eq!(unsafe { take_int_array(input) }, values);
    }

    for len in [0, 1, 8] {
        let output = Java_miri_1tests_Numbers_make(env, class, len);
        // SAFETY: the generated function returned a fresh array owned here.
        assert_eq!(
            unsafe { take_int_array(output) },
            (0..len).collect::<Vec<_>>()
        );
    }
}
