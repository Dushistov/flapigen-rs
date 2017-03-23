# rust-swig [![Build status](https://travis-ci.org/Dushistov/rust_swig.svg?branch=master)](https://travis-ci.org/Dushistov/rust_swig) [![Build status](https://ci.appveyor.com/api/projects/status/db4rs7f96iba4bt8/branch/master?svg=true)](https://ci.appveyor.com/project/Dushistov/rust-swig/branch/master) [![Coverage Status](https://coveralls.io/repos/github/Dushistov/rust_swig/badge.svg?branch=master)](https://coveralls.io/github/Dushistov/rust_swig?branch=master) [![License](https://img.shields.io/badge/license-BSD-green.svg)](https://github.com/Dushistov/rust_swig/blob/master/LICENSE)


## Getting started

Suppose you have the following Rust code:
```Rust
struct Foo {
    data: i32
}

impl Foo {
    fn new(val: i32) -> Foo {
        Foo{data: val}
    }

    fn f(&self, a: i32, b: i32) -> i32 {
        self.data + a + b
    }
}

fn f2(a: i32) -> i32 {
    a * 2
}
```

and you want to write in Java something like this:

```Java
Foo foo = new Foo(5);
int res = foo.f(1, 2);
assert res == 8;
```

In order to implement it rust_swig suggests the following functionality,
in Rust project you write (in Rust language):

```Rust
foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    method Foo::f(&self, _: i32, _: i32) -> i32;
    static_method f2(_: i32) -> i32;
});
```

and that's all, as a result rust_swig generates JNI wrappers for Rust functions
and Java code to call these JNI functions.

See [rust_swig tests for jni](https://github.com/Dushistov/rust_swig/tree/master/jni_tests) for more complex examples.

## Advanced
Also rust_swig support bypassing of code generation:

```Rust
foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    method Foo::f(&self, _: i32, _: i32) -> i32;
    static_method f2(_: i32) -> i32;
    foreigner_code "    public int javaFunc() { return 17; }\n";
    foreigner_code r#"
    public Foo[] testHandArrayReturn() { return do_testHandArrayReturn(this.mNativeObj); }
    private static native Boo[] do_testHandArrayReturn(long me);
"#;
});
```

after that you can implement Java_com_example_TestPathAndResult_do_1testHandArrayReturn
function by your self, usefull when rust_swig can not handle something automaticaly,
or you want something special.

Also you can create alias for function name:

```Rust
foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    method Foo::f(&self, _: i32, _: i32) -> i32; alias getF;
});
```

may be usefull if you want name functions in Java in camel case style,
while want in Rust use snake case style.

