# rust-swig [![Build status](https://travis-ci.org/Dushistov/rust_swig.svg?branch=master)](https://travis-ci.org/Dushistov/rust_swig) [![Build status](https://ci.appveyor.com/api/projects/status/db4rs7f96iba4bt8/branch/master?svg=true)](https://ci.appveyor.com/project/Dushistov/rust-swig/branch/master) [![Build Status](https://dev.azure.com/dushistov/rust_swig/_apis/build/status/Dushistov.rust_swig?branchName=master)](https://dev.azure.com/dushistov/rust_swig/_build/latest?definitionId=2&branchName=master) [![License](https://img.shields.io/badge/license-BSD-green.svg)](https://github.com/Dushistov/rust_swig/blob/master/LICENSE) [![Rust Documentation](https://img.shields.io/badge/api-rustdoc-blue.svg)](https://docs.rs/rust_swig)

Tool for connecting programs or libraries written in Rust with other languages.
Currently implemented support for `C++` and `Java`, but you can write support
for any language of your choice. For an instruction how to integrate rust_swig with your
project look [here](#integration-of-rust_swig-with-your-project).


## Getting started

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
foreigner_class!(class Foo {
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

See [rust_swig tests for jni](https://github.com/Dushistov/rust_swig/tree/master/jni_tests) for more complex examples.

## Advanced
Also rust_swig support bypassing of code generation:

```rust
foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    fn Foo::f(&self, _: i32, _: i32) -> i32;
    fn f2(_: i32) -> i32;
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

```rust
foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
    fn Foo::f(&self, _: i32, _: i32) -> i32; alias getF;
});
```

may be usefull if you want name functions in Java in camel case style,
while want in Rust use snake case style.

Also you can add comments to generated code with Rust's doc comments:

```rust
foreigner_class!(
/// This is class you dream about
class Foo {
    self_type Foo;
    /// Cool constructor
    constructor Foo::new(_: i32) -> Foo;
    fn Foo::f(&self, _: i32, _: i32) -> i32; alias getF;
});
```

Also you can "export" `enum` (`C` like enum) to foreign language:

```rust
enum MyEnum {
    Item1,
    Item2,
    Item3,
}

foreign_enum!(enum MyEnum {
  ITEM1 = MyEnum::Item1,
  ITEM2 = MyEnum::Item2,
  ITEM3 = MyEnum::Item3,
});

foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::default() -> Foo;
    fn f1(&mut self, v: MyEnum);
});
```

after that you can write in Java:

```Java
MyEnum v1 = MyEnum.ITEM1;
Foo foo = new Foo();
foo.f1(v1);
```

or in C++
```C++
MyEnum v1 = ITEM1;
```

Also you can use `trait` to describe callback from Rust to Java/C++:

```rust
trait SomeTrait {
    fn on_state_changed(&self, item: i32, is_ok: bool);
}

foreign_callback!(callback SomeObserver {
    self_type SomeTrait;
    onStateChanged = SomeTrait::on_state_changed(&self, _: i32, _: bool);
});

foreigner_class!(class ClassWithCallbacks {
    self_type Foo;
    constructor Foo::default() -> Foo;
    fn f1(&mut self, cb: Box<SomeTrait>);
});
```

and in Java you can write:

```Java
class Boo implements SomeObserver {
    public void onStateChanged(int item, boolean isOk) {}
}

Foo foo = new Foo();
Boo boo = new Boo();
foo.f1(boo);//we register callback here
```

or in C++:

```C++
class Boo : public SomeObserver {
public:
    void onStateChanged(int, bool) override {}
};
```

Also you can insert rust code into generated code:

```rust
foreigner_class!(class Foo {
    self_type Foo;
    constructor Foo::new(_: i32) -> Foo;
	fn to_string(&self) -> String {
        format!("{}", self)
	}
});
```

## Integration of rust_swig with your project

rust_swig is designed to be used from [cargo build scripts](https://doc.rust-lang.org/cargo/reference/build-scripts.html).
Just copy an appropriate code from examples crates: [general java](https://github.com/Dushistov/rust_swig/tree/master/jni_tests),
[android/java](https://github.com/Dushistov/rust_swig/tree/master/android-example), [c++](https://github.com/Dushistov/rust_swig/tree/master/c%2B%2B_tests) and add rust_swig as `[build-dependencies]` into your `Cargo.toml`.
