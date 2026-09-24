use std::{ffi::c_void, mem::ManuallyDrop, sync::atomic::AtomicI32};

use super::*;

#[test]
fn object_borrows_and_explicit_destruction() {
    let object = Tracked_new(7).cast_mut().cast::<Tracked>();
    // SAFETY: the generated constructor returned a live Tracked allocation.
    // Only the counter is retained; no borrow survives the next glue call.
    let drops = unsafe { Arc::clone(&(*object).drops) };
    assert_eq!(Tracked_value(object), 7);
    Tracked_set_value(object, 42);
    assert_eq!(Tracked_value(object), 42);
    assert_eq!(drops.load(Ordering::SeqCst), 0);
    Tracked_delete(object);
    assert_eq!(drops.load(Ordering::SeqCst), 1);
}

#[test]
fn object_ownership_transfers_back_to_rust() {
    let object = Tracked_new(19).cast_mut().cast::<Tracked>();
    // SAFETY: same constructor contract as above; the borrow ends here.
    let drops = unsafe { Arc::clone(&(*object).drops) };
    assert_eq!(Tracked_take(object.cast()), 19);
    // The consuming call owns destruction: do not call Tracked_delete afterward.
    assert_eq!(drops.load(Ordering::SeqCst), 1);
}

fn assert_string(value: &CRustString, expected: &str) {
    // SAFETY: callers retain ownership of this generated string during the
    // assertion. Even an empty Rust String supplies a non-null data pointer.
    let bytes = unsafe { std::slice::from_raw_parts(value.data.cast::<u8>(), value.len) };
    assert_eq!(bytes, expected.as_bytes());
}

#[test]
fn string_clones_have_independent_ownership() {
    for input in ["", "hello", "Привет\0🦀"] {
        let original = Buffers_make_string(CRustStrView::from_str(input));
        let cloned = crust_string_clone(original);
        assert_string(&original, input);
        crust_string_free(original);
        assert_string(&cloned, input);
        crust_string_free(cloned);
    }
}

#[test]
fn strings_grow_and_transfer_back_to_rust() {
    for input in ["", "a"] {
        let original = Buffers_make_string(CRustStrView::from_str(input));
        let old_capacity = original.capacity;
        let suffix = "β".repeat(old_capacity + 32);
        let grown = crust_string_push_str(original, CRustStrView::from_str(&suffix));
        assert!(grown.capacity > old_capacity);
        let expected = format!("{input}{suffix}");
        assert_string(&grown, &expected);
        assert_eq!(Buffers_take_string(grown), expected.len());
        // Both the old descriptor and the consumed descriptor are now invalid.
    }
}

#[test]
fn vectors_can_be_borrowed_mutated_and_consumed() {
    for len in [0, 1, 8] {
        let vector = Buffers_make_vec(len);
        assert_eq!(vector.len, len as usize);
        let expected: u32 = (0..len).sum();
        assert_eq!(
            Buffers_sum(CRustSliceu32 {
                data: vector.data,
                len: vector.len
            }),
            expected
        );
        Buffers_reverse(CRustSliceMutu32 {
            data: vector.data,
            len: vector.len,
        });
        // SAFETY: this vector is still owned by the test. The previous mutable
        // borrow ended when the generated function returned.
        let actual = unsafe { std::slice::from_raw_parts(vector.data, vector.len) };
        assert_eq!(actual, (0..len).rev().collect::<Vec<_>>());
        assert_eq!(Buffers_take_vec(vector), expected);
    }
}

#[test]
fn vectors_can_be_freed_without_consuming_them_in_a_method() {
    for len in [0, 1, 8] {
        CRustVecu32_free(Buffers_make_vec(len));
    }
}

#[test]
fn opaque_vector_access_restores_ownership() {
    for len in [0, 1, 8] {
        let values: Vec<u32> = (0..len).collect();
        let raw = CRustVecAccess::from_vec(values);
        assert_eq!(
            CRustVecAccess::to_slice::<u32>(raw),
            &(0..len).collect::<Vec<_>>()
        );
        assert_eq!(
            CRustVecAccess::to_vec::<u32>(raw),
            (0..len).collect::<Vec<_>>()
        );
    }
}

#[test]
fn foreign_vector_push_remove_and_free() {
    let mut values = CRustForeignVec::from_vec(vec![Tracked::new(7)]);
    push_foreign_class_to_vec::<Tracked>(&mut values, Tracked::box_object(Tracked::new(8)));
    assert_eq!(values.len, 2);

    let removed = remove_foreign_class_from_vec::<Tracked>(&mut values, 0);
    let removed = Tracked::unbox_object(removed);
    assert_eq!(removed.value(), 7);
    drop(removed);
    assert_eq!(values.len, 1);

    drop_foreign_class_vec::<Tracked>(values);
}

#[test]
fn borrowed_slices_remain_owned_by_the_caller() {
    for len in [0, 1, 8] {
        let mut values: Vec<u32> = (0..len).collect();
        Buffers_reverse(CRustSliceMutu32 {
            data: values.as_mut_ptr(),
            len: values.len(),
        });
        assert_eq!(values, (0..len).rev().collect::<Vec<_>>());
        assert_eq!(
            Buffers_sum(CRustSliceu32 {
                data: values.as_ptr(),
                len: values.len()
            }),
            (0..len).sum()
        );
        values.push(100);
        assert_eq!(values.pop(), Some(100));
    }
}

#[test]
fn null_empty_foreign_slices_are_valid_inputs() {
    assert_eq!(
        Buffers_sum_tracked(CRustObjectSlice {
            data: std::ptr::null(),
            len: 0,
            step: 0,
        }),
        0
    );
    assert_eq!(
        Buffers_increment_tracked(CRustObjectMutSlice {
            data: std::ptr::null_mut(),
            len: 0,
            step: 0,
        }),
        0
    );
}

#[test]
fn null_empty_primitive_slice() {
    assert_eq!(
        Buffers_sum(CRustSliceu32 {
            data: std::ptr::null(),
            len: 0
        }),
        0
    );
}

#[test]
fn null_empty_mutable_primitive_slice() {
    Buffers_reverse(CRustSliceMutu32 {
        data: std::ptr::null_mut(),
        len: 0,
    });
}

#[test]
fn null_empty_string_view() {
    let string = Buffers_make_string(CRustStrView {
        data: std::ptr::null(),
        len: 0,
    });
    assert_string(&string, "");
    crust_string_free(string);
}

#[test]
fn null_empty_string_view_append() {
    for input in ["", "hello"] {
        let string = Buffers_make_string(CRustStrView::from_str(input));
        let string = crust_string_push_str(
            string,
            CRustStrView {
                data: std::ptr::null(),
                len: 0,
            },
        );
        assert_string(&string, input);
        crust_string_free(string);
    }
}

#[test]
fn null_empty_vector_access_slice() {
    // Models a default or moved-from RustVecAccess; there is no allocation to reclaim.
    let descriptor = CRustVecAccess {
        data: std::ptr::null_mut(),
        len: 0,
        capacity: 0,
    };
    assert!(CRustVecAccess::to_slice::<u32>(descriptor).is_empty());
}

#[derive(Default)]
struct CallbackState {
    calls: AtomicUsize,
    total: AtomicI32,
    drops: AtomicUsize,
}

struct CallbackContext {
    state: Arc<CallbackState>,
}

impl Drop for CallbackContext {
    fn drop(&mut self) {
        self.state.drops.fetch_add(1, Ordering::SeqCst);
    }
}

fn callback_context(state: &Arc<CallbackState>) -> *const c_void {
    Box::into_raw(Box::new(CallbackContext {
        state: Arc::clone(state),
    }))
    .cast()
}

extern "C" fn on_value(value: i32, opaque: *const c_void) {
    // SAFETY: the test installs this function with a live CallbackContext;
    // invocation only borrows it, and the callback destructor owns reclamation.
    let context = unsafe { &*opaque.cast::<CallbackContext>() };
    context.state.calls.fetch_add(1, Ordering::SeqCst);
    context.state.total.fetch_add(value, Ordering::SeqCst);
}

extern "C" fn total(opaque: *const c_void) -> i32 {
    // SAFETY: same live opaque context contract as on_value.
    let context = unsafe { &*opaque.cast::<CallbackContext>() };
    context.state.total.load(Ordering::SeqCst)
}

extern "C" fn destroy_callback(opaque: *const c_void) {
    // SAFETY: this receives the Box::into_raw pointer exactly once, after the
    // last callback invocation, through the generated adapter's destructor.
    drop(unsafe { Box::from_raw(opaque.cast_mut().cast::<CallbackContext>()) });
}

extern "C" fn finish(value: i32, opaque: *const c_void) {
    on_value(value, opaque);
    // A consuming C++ callback takes responsibility for deleting its context.
    destroy_callback(opaque);
}

#[test]
fn boxed_callback_is_invoked_and_destroyed_once() {
    let state = Arc::new(CallbackState::default());
    // The real C++ caller supplies a POD function table. Suppress the Rust
    // table's Drop: the generated glue clones it into the owning Rust adapter.
    let callback = ManuallyDrop::new(C_Observer {
        opaque: callback_context(&state),
        C_Observer_deref: destroy_callback,
        on_value,
        total,
    });
    assert_eq!(Callbacks_notify(&*callback), 30);
    assert_eq!(state.calls.load(Ordering::SeqCst), 2);
    assert_eq!(state.drops.load(Ordering::SeqCst), 1);
}

#[test]
fn consuming_callback_does_not_run_the_destructor_again() {
    let state = Arc::new(CallbackState::default());
    let callback = C_Completion {
        opaque: callback_context(&state),
        C_Completion_deref: destroy_callback,
        finish,
    };
    Completion::finish(callback, 73);
    assert_eq!(state.calls.load(Ordering::SeqCst), 1);
    assert_eq!(state.total.load(Ordering::SeqCst), 73);
    assert_eq!(state.drops.load(Ordering::SeqCst), 1);
}

#[test]
fn abandoned_consuming_callback_destroys_its_context() {
    let state = Arc::new(CallbackState::default());
    let callback = C_Completion {
        opaque: callback_context(&state),
        C_Completion_deref: destroy_callback,
        finish,
    };
    drop(callback);
    assert_eq!(state.calls.load(Ordering::SeqCst), 0);
    assert_eq!(state.drops.load(Ordering::SeqCst), 1);
}
