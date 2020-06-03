
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

// In .NET P/Invoke, bool is by default mapped to WIN32 BOOL, which is an alias for int.
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

#[allow(dead_code)]
pub trait SwigForeignClass {
    // fn c_class_name() -> *const ::std::os::raw::c_char;
    fn box_object(x: Self) -> *mut ::std::os::raw::c_void;
    fn unbox_object(p: *mut ::std::os::raw::c_void) -> Self;
}
