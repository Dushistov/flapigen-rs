# rust-swig [![Build Status](https://dev.azure.com/dushistov/rust_swig/_apis/build/status/Dushistov.rust_swig?branchName=master)](https://dev.azure.com/dushistov/rust_swig/_build/latest?definitionId=2&branchName=master) [![License](https://img.shields.io/badge/license-BSD-green.svg)](https://github.com/Dushistov/rust_swig/blob/master/LICENSE) [![Rust Documentation](https://img.shields.io/badge/api-rustdoc-blue.svg)](https://docs.rs/rust_swig)

Tool for connecting programs or libraries written in Rust with other languages.
Currently implemented support for `C++` and `Java`, but you can write support
for any language of your choice. For an instruction how to integrate rust_swig with your
project look [here](#integration-of-rust_swig-with-your-project).

Suppose you have the following Rust code:

```rust
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

    fn set_field(&mut self, v: i32) {
        self.data = v;
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
or in C++ something like this:

```C++
Foo foo(5);
int res = foo.f(1, 2);
assert(res == 8);
```

In order to implement it rust_swig suggests the following functionality,
in Rust project you write (in Rust language):

```rust
foreign_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    fn Foo::set_field(&mut self, _: i32);
    fn Foo::f(&self, _: i32, _: i32) -> i32;
    fn f2(_: i32) -> i32;
});
```

and that's all, as a result rust_swig generates JNI wrappers for Rust functions
and Java code to call these JNI functions
or generates C compatible wrappers in case of C++ and
C++ code to call these C functions.

## Users Guide

[📚 Read the `rust_swig` users guide here! 📚](https://dushistov.github.io/rust_swig/)

