
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

foreign_typemap!(
    (r_type) isize;
    (f_type) "IntPtr";
);

