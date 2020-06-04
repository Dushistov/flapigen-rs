
// It is currently unused.
mod swig_foreign_types_map {}

foreign_typemap!(
    (r_type) f32;
    (f_type) "float";
);

foreign_typemap!(
    (r_type) f64;
    (f_type) "double";
);

foreign_typemap!(
    (r_type) ();
    (f_type) "void";
);

foreign_typemap!(
    (r_type) i8;
    (f_type) "sbyte";
);

foreign_typemap!(
    (r_type) u8;
    (f_type) "byte";
);

foreign_typemap!(
    (r_type) i16;
    (f_type) "short";
);

foreign_typemap!(
    (r_type) u16;
    (f_type) "ushort";
);

foreign_typemap!(
    (r_type) i32;
    (f_type) "int";
);

foreign_typemap!(
    (r_type) u32;
    (f_type) "uint";
);

foreign_typemap!(
    (r_type) i64;
    (f_type) "long";
);

foreign_typemap!(
    (r_type) u64;
    (f_type) "ulong";
);

foreign_typemap!(
    (r_type) usize;
    (f_type) "UIntPtr";
);

// foreign_typemap!(
//     (r_type) isize;
//     (f_type) "IntPtr";
// );

// foreign_typemap!(
//     (r_type) *mut std::os::raw::c_void;
//     (f_type) "IntPtr";
// );

foreign_typemap!(
    (r_type) /* c_str_u16 */ *mut u16;
    (f_type) "/* mut c_str_u16 */ IntPtr";
);

foreign_typemap!(
    (r_type) /* c_str_u16 */ *const u16;
    (f_type) "/* const c_str_u16 */ IntPtr";
);

// .NET prefers UTF16, but Rust doesn't provide CString/OSString equivalent that supports UTF16 on Linux.
// We need to go a bit lower.

unsafe fn c_str_u16_len(mut c_str_u16_ptr: *const u16) -> usize {
    let mut len = 0;
    while *c_str_u16_ptr != 0 {
        len += 1;
        c_str_u16_ptr = c_str_u16_ptr.offset(1);
    }
    len
}

unsafe fn c_str_u16_to_string(c_str_u16_ptr: *const u16) -> String {
    let len = c_str_u16_len(c_str_u16_ptr);
    let slice = std::slice::from_raw_parts(c_str_u16_ptr, len);
    String::from_utf16_lossy(slice)
}

fn alloc_c_str_u16(string: &str) -> *const u16 {
    let mut bytes_vec: Vec<u16> = string.encode_utf16().collect();
    // Add terminate NULL character
    bytes_vec.push(0);
    let boxed_slice = bytes_vec.into_boxed_slice();
    let slice_ptr = Box::into_raw(boxed_slice);
    unsafe {
        (*slice_ptr).as_ptr()
    }
}

#[allow(non_snake_case)]
#[no_mangle]
unsafe extern "C" fn String_delete(c_str_u16: *mut u16) {
    let size = c_str_u16_len(c_str_u16) + 1; // Add NULL character size.
    let slice_ptr = std::ptr::slice_from_raw_parts_mut(c_str_u16, size);
    let boxed_slice: Box<[u16]> = Box::from_raw(slice_ptr);
    std::mem::drop(boxed_slice);
}

foreign_typemap!(
    ($p:r_type) bool => u8 {
        $out = if $p  { 1 } else { 0 };
    };
    ($p:f_type) => "bool" "($p != 0)";
    ($p:r_type) bool <= u8 {
        $out = $p != 0;
    };
    ($p:f_type) <= "bool" "$p ? 1 : 0";
);

foreign_typemap!(
    ($p:r_type) String => *const u16 {
        $out = alloc_c_str_u16(&$p);
    };
    ($p:f_type) => "string" "Marshal.PtrToStringUni($p); RustInterop.String_delete($p)";
    ($p:r_type) String <= *const u16 {
        $out = unsafe { c_str_u16_to_string($p) };
    };
    ($p:f_type, finalizer="Marshal.FreeHGlobal({to_var});") <= "string" "Marshal.StringToHGlobalUni($p)";
);

#[allow(dead_code)]
pub trait SwigForeignClass {
    // fn c_class_name() -> *const ::std::os::raw::c_char;
    fn box_object(x: Self) -> *mut ::std::os::raw::c_void;
    fn unbox_object(p: *mut ::std::os::raw::c_void) -> Self;
}
