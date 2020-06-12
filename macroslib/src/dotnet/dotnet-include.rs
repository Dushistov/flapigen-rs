
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

foreign_typemap!(
    ($p:r_type) &str => /* &str */ *const u16 {
        $out = alloc_c_str_u16(&$p);
    };
    ($p:f_type) => "/* &str */ string" "Marshal.PtrToStringUni($p); RustInterop.String_delete($p)";
    ($p:r_type) &str <= /* &str */ *const u16 {
        $out = unsafe { &c_str_u16_to_string($p) };
    };
    ($p:f_type, finalizer="Marshal.FreeHGlobal({to_var});") <= "/* &str */ string" "Marshal.StringToHGlobalUni($p)";
);

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
        unsafe extern "C" fn RustOptionT_new_none!()() -> *mut Option<swig_subst_type!(T)> {
            Box::into_raw(Box::new(None))
        }

        #[allow(non_snake_case)]
        #[no_mangle]
        unsafe extern "C" fn RustOptionT_new_some!()(value_0: swig_i_type!(T)) -> *mut Option<swig_subst_type!(T)> {
            swig_from_i_type_to_rust!(T, value_0, value_1);
            Box::into_raw(Box::new(Some(value_1)))
        }

        #[allow(non_snake_case)]
        #[no_mangle]
        unsafe extern "C" fn RustOptionT_is_some!()(opt: *mut Option<swig_subst_type!(T)>) -> u8 {
            if (*opt).is_some() { 1 } else { 0 }
        }

        #[allow(non_snake_case)]
        #[no_mangle]
        unsafe extern "C" fn RustOptionT_take!()(opt: *mut Option<swig_subst_type!(T)>) -> swig_i_type!(T) {
            let ret_0 = Box::from_raw(opt).expect("RustOptionT_take!(): trying to take the value from Option::None");
            swig_from_rust_to_i_type!(T, ret_0, ret_1);
            ret_1
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
        $out = Box::into_raw(Box::new($p)) as *mut ::std::os::raw::c_void;
    };
    ($p:f_type) => "Option<swig_f_type!(T)>" "RustOptionT!().rust_to_dotnet($p)";
    ($p:r_type) <T> Option<T> <= /* Option */ *mut ::std::os::raw::c_void {
        $out = unsafe { (*($p as *mut Option<swig_subst_type!(T)>)).clone() };
    };
    ($p:f_type) <= "Option<swig_f_type!(T)>" "RustOptionT!().dotnet_to_rust($p)";

);

foreign_typemap!(
    ($p:r_type) /* Vec */ *mut ::std::os::raw::c_void;
    ($p:f_type) "/* RustVec */ IntPtr";
);

foreign_typemap!(
    generic_alias!(RustVecT = swig_concat_idents!(RustVec, swig_f_type!(T)));
    generic_alias!(RustVecT_new = swig_concat_idents!(RustVec, swig_f_type!(T), _new));
    generic_alias!(RustVecT_delete = swig_concat_idents!(RustVec, swig_f_type!(T), _delete));
    generic_alias!(RustVecT_index = swig_concat_idents!(RustVec, swig_f_type!(T), _index));
    generic_alias!(RustVecT_index_set = swig_concat_idents!(RustVec, swig_f_type!(T), _index_set));
    generic_alias!(RustVecT_len = swig_concat_idents!(RustVec, swig_f_type!(T), _len));
    generic_alias!(RustVecT_remove = swig_concat_idents!(RustVec, swig_f_type!(T), _remove));
    generic_alias!(RustVecT_insert = swig_concat_idents!(RustVec, swig_f_type!(T), _insert));
    generic_alias!(RustVecT_to_list = swig_concat_idents!(RustVec, swig_f_type!(T), _to_list));
    // generic_alias!(RustVecT_push = swig_concat_idents!(RustVec, swig_f_type!(T), _new));

    ($p:r_type) <T> Vec<T> => /* Vec */ *mut ::std::os::raw::c_void {
        $out = Box::into_raw(Box::new($p)) as *mut ::std::os::raw::c_void;
    };
    ($p:f_type) => "RustVecT!()" "new RustVecT!()($p)";
    ($p:r_type) <T> Vec<T> <= /* Vec */ *mut ::std::os::raw::c_void {
        $out = unsafe { &*($p as *mut Vec<swig_subst_type!(T)>) }.clone();
    };
    ($p:f_type) <= "RustVecT!()" "$p.nativePtr";

    define_c_type!(
        module = "RustVecT!()";

        #[allow(non_snake_case)]
        #[no_mangle]
        unsafe extern "C" fn RustVecT_new!()() -> *mut Vec<swig_subst_type!(T)> {
            Box::into_raw(Box::new(Vec::new()))
        }

        #[allow(non_snake_case)]
        #[no_mangle]
        unsafe extern "C" fn RustVecT_delete!()(vec: *mut Vec<swig_subst_type!(T)>) {
            ::std::mem::drop(Box::from_raw(vec))
        }

        #[allow(non_snake_case)]
        #[no_mangle]
        unsafe extern "C" fn RustVecT_index!()(vec: *mut Vec<swig_subst_type!(T)>, index: usize) -> swig_i_type!(T) {
            let ret_0 = (*vec)[index];
            swig_from_rust_to_i_type!(T, ret_0, ret_1);
            ret_1
        }

        #[allow(non_snake_case)]
        #[no_mangle]
        unsafe extern "C" fn RustVecT_index_set!()(vec: *mut Vec<swig_subst_type!(T)>, index: usize, element_0: swig_i_type!(T)) {
            swig_from_i_type_to_rust!(T, element_0, element_1);
            (*vec)[index] = element_1;
        }

        #[allow(non_snake_case)]
        #[no_mangle]
        unsafe extern "C" fn RustVecT_len!()(vec: *mut Vec<swig_subst_type!(T)>) -> usize {
            (*vec).len()
        }

        #[allow(non_snake_case)]
        #[no_mangle]
        unsafe extern "C" fn RustVecT_remove!()(vec: *mut Vec<swig_subst_type!(T)>, index: usize) -> swig_i_type!(T) {
            let ret_0 = (*vec).remove(index);
            swig_from_rust_to_i_type!(T, ret_0, ret_1);
            ret_1
        }

        #[allow(non_snake_case)]
        #[no_mangle]
        unsafe extern "C" fn RustVecT_insert!()(vec: *mut Vec<swig_subst_type!(T)>, index: usize, element_0: swig_i_type!(T)) {
            swig_from_i_type_to_rust!(T, element_0, element_1);
            (*vec).insert(index, element_1);
        }

        // #[allow(non_snake_case)]
        // #[no_mangle]
        // unsafe extern "C" fn IterT_next!()(vec: *mut std::slice::Iter<swig_subst_type!(T)>) -> swig_i_type!(T) {
        //     swig_from_i_type_to_rust!(T, element_0, element_1);
        //     (*vec).insert(index, element_1);
        // }
    );

    foreigner_code!(
        module = "RustVecT!()";
        r#"
    public class RustVecT!(): IDisposable {
        [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
        internal static extern IntPtr RustVecT_new!()();
        
        [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
        internal static extern void RustVecT_delete!()(IntPtr vecPtr);

        [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
        internal static extern swig_i_type!(T) RustVecT_index!()(IntPtr vecPtr, UIntPtr index);

        [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
        internal static extern void RustVecT_index_set!()(IntPtr vecPtr, UIntPtr index, swig_i_type!(T) element);

        [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
        internal static extern UIntPtr RustVecT_len!()(IntPtr vecPtr);

        [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
        internal static extern swig_i_type!(T) RustVecT_remove!()(IntPtr vecPtr, UIntPtr index);

        [DllImport("{native_lib_name}", CallingConvention = CallingConvention.Cdecl)]
        internal static extern void RustVecT_insert!()(IntPtr vecPtr, UIntPtr index, swig_i_type!(T) element);

        internal IntPtr nativePtr;

        internal RustVecT!()(IntPtr nativePtr) {
            this.nativePtr = nativePtr;
        }

        public RustVecT!()() {
            nativePtr = RustVecT!().RustVecT_new!()();
        }

        public swig_f_type!(T) this[int index] {
            get {
                var ret = RustVecT!().RustVecT_index!()(nativePtr, (UIntPtr)index);
                return swig_foreign_from_i_type!(T, ret);
            }
            set {
                var value_1 = swig_foreign_to_i_type!(T, value);
                RustVecT_index_set!()(nativePtr, (UIntPtr)index, value_1);
            }
        }

        public swig_f_type!(T) Remove(int index)
        {
            var ret = RustVecT!().RustVecT_remove!()(nativePtr, (UIntPtr)index);
            return swig_foreign_from_i_type!(T, ret);
        }

        public void Insert(int index, swig_f_type!(T) element)
        {
            var element_1 = swig_foreign_to_i_type!(T, element);
            RustVecT!().RustVecT_insert!()(nativePtr, (UIntPtr)index, element_1);
        }

        public void Add(swig_f_type!(T) element)
        {
            Insert(Count, element);
        }

        public int Count {
            get {
                return (int)RustVecT!().RustVecT_len!()(nativePtr);
            }
        }

        public void Dispose() {
            DoDispose();
            GC.SuppressFinalize(this);
        }

        private void DoDispose() {
            if (nativePtr != IntPtr.Zero) {{
                RustVecT_delete!()(nativePtr);
                nativePtr = IntPtr.Zero;
            }}
        }

        ~RustVecT!()() {
            DoDispose();
        }

    }
        "#
    );
);
