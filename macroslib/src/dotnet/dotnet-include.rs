
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


#[allow(dead_code)]
#[swig_code = "let mut {to_var}: {to_var_type} = {from_var}.swig_into();"]
trait SwigInto<T> {
    fn swig_into(self) -> T;
}

#[allow(dead_code)]
#[swig_code = "let mut {to_var}: {to_var_type} = <{to_var_type}>::swig_from({from_var});"]
trait SwigFrom<T> {
    fn swig_from(_: T) -> Self;
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

// impl<T: SwigForeignClass> SwigDeref for T {
//     type Target = T;
//     fn swig_deref(&self) -> &T {
//         self
//     }
// }

// impl<T: SwigForeignClass> SwigDerefMut for T {
//     type Target = T;
//     fn swig_deref_mut(&mut self) -> &mut T {
//         self
//     }
// }

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
    if c_str_u16_ptr.is_null() {
        return String::new();
    }
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
    ($p:r_type) String => /* String */ *const u16 {
        $out = alloc_c_str_u16(&$p);
    };
    ($p:f_type) => "string" "Marshal.PtrToStringUni($p); RustInterop.String_delete($p)";
    ($p:r_type) String <= /* String */ *const u16 {
        $out = unsafe { c_str_u16_to_string($p) };
    };
    ($p:f_type, finalizer="Marshal.FreeHGlobal({to_var});") <= "string" "Marshal.StringToHGlobalUni($p)";
);


impl SwigDeref for String {
    type Target = str;
    fn swig_deref(&self) -> &str {
        &self
    }
}

impl<T> SwigDeref for Vec<T> {
    type Target = &[T];
    fn swig_deref(&self) -> &[T] {
        &self
    }
}

impl SwigInto<String> for &str {
    fn swig_into(self) -> String {
        self.to_owned()
    }
}

impl<T> SwigInto<Vec<T>> for &[T] {
    fn swig_into(self) -> Vec<T> {
        self.to_owned()
    }
}

#[allow(dead_code)]
pub trait SwigForeignClass: Sized {
    type StorageType: SwigForeignClassStorage<BaseType=Self>;
    // fn c_class_name() -> *const ::std::os::raw::c_char;
    // fn box_object(x: Self) -> *mut ::std::os::raw::c_void;
    // fn unbox_object(p: *mut ::std::os::raw::c_void) -> Self;
    fn swig_into_storage_type(self) -> Self::StorageType;
}

pub trait SwigForeignClassStorage: Sized {
    type BaseType: SwigForeignClass;

    fn swig_as_ref(&self) -> &Self::BaseType;
    fn swig_as_mut(&mut self) -> &mut Self::BaseType;
    fn swig_cloned(&self) -> Self::BaseType;
    fn swig_leak_into_raw(self) -> *mut Self;
    fn swig_drop_raw(raw_ptr: *mut Self);
}

foreign_typemap!(
    ($p:r_type) /* Option */ *mut ::std::os::raw::c_void;
    ($p:f_type) "/* Option */ IntPtr";
);


foreign_typemap!(
    generic_alias!(RustOptionT = swig_concat_idents!(RustOption, swig_f_type!(T)));
    generic_alias!(RustOptionT_new_none = swig_concat_idents!(RustOption, swig_f_type!(T), _new_none));
    generic_alias!(RustOptionT_new_some = swig_concat_idents!(RustOption, swig_f_type!(T), _new_some));
    generic_alias!(RustOptionT_is_some = swig_concat_idents!(RustOption, swig_f_type!(T), _is_some));
    generic_alias!(RustOptionT_take = swig_concat_idents!(RustOption, swig_f_type!(T), _take));
    // generic_alias!(RustOptionT_delete = swig_concat_idents!(RustOption, swig_f_type!(T), _delete));

    define_c_type!(
        module = "RustOptionT!()";

        // #[allow(non_snake_case)]
        // #[no_mangle]
        // unsafe extern "C" fn RustOptionT_delete!()(opt: *mut Option<swig_subst_type!(T)>) {
        //     ::std::mem::drop(Box::from_raw(opt))
        // }

        #[allow(non_snake_case)]
        #[no_mangle]
        unsafe extern "C" fn RustOptionT_new_none!()() -> *mut Option<swig_i_type!(T)> {
            Box::into_raw(Box::new(None))
        }

        #[allow(non_snake_case)]
        #[no_mangle]
        unsafe extern "C" fn RustOptionT_new_some!()(value_0: swig_i_type!(T)) -> *mut Option<swig_i_type!(T)> {
            //swig_from_i_type_to_rust!(T, value_0, value_1);
            Box::into_raw(Box::new(Some(value_0)))
        }

        #[allow(non_snake_case)]
        #[no_mangle]
        unsafe extern "C" fn RustOptionT_is_some!()(opt: *mut Option<swig_i_type!(T)>) -> u8 {
            if (*opt).is_some() { 1 } else { 0 }
        }

        #[allow(non_snake_case)]
        #[no_mangle]
        unsafe extern "C" fn RustOptionT_take!()(opt: *mut Option<swig_i_type!(T)>) -> swig_i_type!(T) {
            let ret_0 = Box::from_raw(opt).expect("RustOptionT_take!(): trying to take the value from Option::None");
            // swig_from_rust_to_i_type!(T, ret_0, ret_1);
            ret_0
        }
    );

    foreigner_code!(
        module = "Option<T>";
        r#"

        public class Option<T> {
        
            [System.Serializable]
            public class OptionNoneException : System.Exception
            {
                public OptionNoneException() :
                    base("Trying to get the value of an `Option` that is `None`") 
                {
                }
            }
        
            private T value;
            private bool isSome;
        
            public bool IsSome
            {
                get
                {
                    return isSome;
                }
            }
        
            public T Value
            {
                get {
                    if (!isSome) {
                        throw new OptionNoneException();
                    }
                    return value;
                }
            }
        
            public Option()
            {
                value = default(T);
                isSome = false;
            }
        
            public Option(T value)
            {
                if (value == null) 
                {
                    this.value = value;
                    this.isSome = false;
                }
                else
                {
                    this.value = value;
                    this.isSome = true;
                }
            }
        }        
        "#
    );

    foreigner_code!(
        module = "RustOptionT!()";
        r#"
    internal static class RustOptionT!() {
        [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
        internal static extern IntPtr RustOptionT_new_none!()();

        [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
        internal static extern IntPtr RustOptionT_new_some!()(swig_i_type!(T) value);
        
        [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
        internal static extern swig_i_type!(T) RustOptionT_take!()(IntPtr optPtr);

        [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
        internal static extern byte RustOptionT_is_some!()(IntPtr optPtr);

        internal static Option<swig_f_type!(T)> rust_to_dotnet(IntPtr optPtr)
        {
            if (RustOptionT_is_some!()(optPtr) != 0)
            {
                var value_0 = RustOptionT_take!()(optPtr);
                var value_1 = swig_foreign_from_i_type!(T, value_0);
                return new Option<swig_f_type!(T)>(value_1);
            }
            else
            {
                return new Option<swig_f_type!(T)>();
            }
        }

        internal static IntPtr dotnet_to_rust(Option<swig_f_type!(T)> opt)
        {
            if (opt.IsSome)
            {
                var value_0 = swig_foreign_to_i_type!(T, opt.Value);
                return RustOptionT_new_some!()(value_0);                
            }
            else
            {
                return RustOptionT_new_none!()();
            }
        }
    }
    "#);

    ($p:r_type) <T> Option<T> => /* Option */ *mut ::std::os::raw::c_void {
        let $p = $p.map(|value_0| {
            swig_from_rust_to_i_type!(T, value_0, value_1);
            value_1
        });
        $out = Box::into_raw(Box::new($p)) as *mut ::std::os::raw::c_void;
    };
    ($p:f_type) => "Option<swig_f_type!(T)>" "RustOptionT!().rust_to_dotnet($p)";
    ($p:r_type) <T> Option<T> <= /* Option */ *mut ::std::os::raw::c_void {
        let $p = unsafe { Box::from_raw($p as *mut Option<swig_i_type!(T)>) };
        $out = $p.map(|value_0| {
            swig_from_i_type_to_rust!(T, value_0, value_1);
            value_1
        });
    };
    ($p:f_type) <= "Option<swig_f_type!(T)>" "RustOptionT!().dotnet_to_rust($p)";

);

foreign_typemap!(
    ($p:r_type) /* Vec */ *mut ::std::os::raw::c_void;
    ($p:f_type) "/* RustVec */ IntPtr";
);

foreign_typemap!(
    ($p:r_type) /* Iter */ *mut ::std::os::raw::c_void;
    ($p:f_type) "/* Iter */ IntPtr";
);

foreign_typemap!(
    generic_alias!(RustVecT = swig_concat_idents!(RustVec, swig_f_type!(T)));
    generic_alias!(RustVecT_new = swig_concat_idents!(RustVec, swig_f_type!(T), _new));
    generic_alias!(RustVecT_push = swig_concat_idents!(RustVec, swig_f_type!(T), _push));
    generic_alias!(RustVecT_iter_next = swig_concat_idents!(RustVec, swig_f_type!(T), _iter_next));
    generic_alias!(RustVecT_iter_delete = swig_concat_idents!(RustVec, swig_f_type!(T), _iter_delete));
    generic_alias!(RustOptionT = swig_concat_idents!(RustOption, swig_f_type!(T)));

    ($p:r_type) <T> Vec<T> => /* Iter */ *mut ::std::os::raw::c_void {
        let $p: Vec<swig_i_type!(T)> = $p.into_iter().map(|e_0| {
            swig_from_rust_to_i_type!(T, e_0, e_1);
            e_1
        }).collect();
        let $p: std::vec::IntoIter<swig_i_type!(T)> = $p.into_iter();
        $out = Box::into_raw(Box::new($p)) as *mut ::std::os::raw::c_void;
    };
    ($p:f_type) => "System.Collections.Generic.List<swig_f_type!(T)>" r#"new System.Collections.Generic.List<swig_f_type!(T)>();
            while (true)
            {
                var next_rust_opt = RustVecT!().RustVecT_iter_next!()($p);
                var next_opt = RustOptionT!().rust_to_dotnet(next_rust_opt);
                if (!next_opt.IsSome)
                {
                    break;
                }
                $out.Add(next_opt.Value);
            }
            RustVecT!().RustVecT_iter_delete!()($p);
    "#;
    ($p:r_type) <T> Vec<T> <= /* Vec */ *mut ::std::os::raw::c_void {
        let $p: Vec<swig_subst_type!(T)> = unsafe { *Box::from_raw($p as *mut Vec<swig_i_type!(T)>) };
        $out = $p.into_iter().map(|e_0| {
            swig_from_i_type_to_rust!(T, e_0, e_1);
            e_1
        }).collect();
    };
    ($p:f_type) <= "System.Collections.Generic.List<swig_f_type!(T)>" r#"RustVecT!().RustVecT_new!()();
            foreach (var element in $p)
            {
                var i_element = swig_foreign_to_i_type!(T, element);
                RustVecT!().RustVecT_push!()($out, i_element);
            }
    "#;

    define_c_type!(
        module = "RustVecT!()";

        #[allow(non_snake_case)]
        #[no_mangle]
        unsafe extern "C" fn RustVecT_new!()() -> *mut Vec<swig_i_type!(T)> {
            Box::into_raw(Box::new(Vec::new()))
        }

        #[allow(non_snake_case)]
        #[no_mangle]
        unsafe extern "C" fn RustVecT_push!()(vec: *mut Vec<swig_i_type!(T)>, element: swig_i_type!(T)) {
            assert!(!vec.is_null());
            (*vec).push(element);
        }

        #[allow(non_snake_case)]
        #[no_mangle]
        unsafe extern "C" fn RustVecT_iter_next!()(iter: *mut std::vec::IntoIter<swig_i_type!(T)>) -> *mut Option<swig_i_type!(T)> {
            assert!(!iter.is_null());
            let mut iter = &mut *iter;
            Box::into_raw(Box::new(iter.next()))
        }

        #[allow(non_snake_case)]
        #[no_mangle]
        unsafe extern "C" fn RustVecT_iter_delete!()(iter: *mut std::vec::IntoIter<swig_i_type!(T)>) {
            assert!(!iter.is_null());
            ::std::mem::drop(Box::from_raw(iter));
        }
    );

    foreigner_code!(
        module = "RustVecT!()";
        r#"
    public static class RustVecT!() {
        [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
        internal static extern IntPtr RustVecT_new!()();
        
        [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
        internal static extern void RustVecT_push!()(IntPtr vecPtr, swig_i_type!(T) element);

        [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
        internal static extern /* Option<i_type> */ IntPtr RustVecT_iter_next!()(IntPtr iterPtr);
        [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
        internal static extern void RustVecT_iter_delete!()(IntPtr iterPtr);
    }
        "#
    );
);

// foreign_typemap!(
//     generic_alias!(RustVecT = swig_concat_idents!(RustVec, swig_f_type!(T)));
//     generic_alias!(RustVecT_new = swig_concat_idents!(RustVec, swig_f_type!(T), _new));
//     generic_alias!(RustVecT_push = swig_concat_idents!(RustVec, swig_f_type!(T), _push));
//     generic_alias!(RustOptionT = swig_concat_idents!(RustOption, swig_f_type!(T)));

//     ($p:r_type) <T> &[T] <= /* Slice */ *mut ::std::os::raw::c_void {
//         let $p: Vec<swig_subst_type!(T)> = unsafe { *Box::from_raw($p as *mut Vec<swig_i_type!(T)>) };
//         $out = $p.into_iter().map(|e_0| {
//             swig_from_i_type_to_rust!(T, e_0, e_1);
//             e_1
//         }).collect();
//     };
//     ($p:f_type) <= "System.Collections.Generic.List<swig_f_type!(T)>" r#"RustVecT!().RustVecT_new!()();
//             foreach (var element in $p)
//             {
//                 var i_element = swig_foreign_to_i_type!(T, element);
//                 RustVecT!().RustVecT_push!()($out, i_element);
//             }
//     "#;

//     define_c_type!(
//         module = "RustVecT!()";

//         #[allow(non_snake_case)]
//         #[no_mangle]
//         unsafe extern "C" fn RustVecT_new!()() -> *mut Vec<swig_i_type!(T)> {
//             Box::into_raw(Box::new(Vec::new()))
//         }

//         #[allow(non_snake_case)]
//         #[no_mangle]
//         unsafe extern "C" fn RustVecT_push!()(vec: *mut Vec<swig_i_type!(T)>, element: swig_i_type!(T)) {
//             assert!(!vec.is_null());
//             (*vec).push(element);
//         }

//         #[allow(non_snake_case)]
//         #[no_mangle]
//         unsafe extern "C" fn RustVecT_iter_next!()(iter: *mut std::vec::IntoIter<swig_i_type!(T)>) -> *mut Option<swig_i_type!(T)> {
//             assert!(!iter.is_null());
//             let mut iter = &mut *iter;
//             Box::into_raw(Box::new(iter.next()))
//         }

//         #[allow(non_snake_case)]
//         #[no_mangle]
//         unsafe extern "C" fn RustVecT_iter_delete!()(iter: *mut std::vec::IntoIter<swig_i_type!(T)>) {
//             assert!(!iter.is_null());
//             ::std::mem::drop(Box::from_raw(iter));
//         }
//     );

//     foreigner_code!(
//         module = "RustVecT!()";
//         r#"
//     public static class RustVecT!() {
//         [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
//         internal static extern IntPtr RustVecT_new!()();
        
//         [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
//         internal static extern void RustVecT_push!()(IntPtr vecPtr, swig_i_type!(T) element);

//         [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
//         internal static extern /* Option<i_type> */ IntPtr RustVecT_iter_next!()(IntPtr iterPtr);
//         [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
//         internal static extern void RustVecT_iter_delete!()(IntPtr iterPtr);
//     }
//         "#
//     );
// );
