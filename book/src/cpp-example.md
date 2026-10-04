# C++

To run the cpp-example from [here](https://github.com/Dushistov/flapigen-rs/tree/master/cpp-example) you
need [boost](https://www.boost.org/), [cmake](https://cmake.org/) and a C++11 compatible compiler.

## Project Structure

The projects consists of two parts: [cpp-part](https://github.com/Dushistov/flapigen-rs/tree/master/cpp-example/cpp-part)
and [rust-part](https://github.com/Dushistov/flapigen-rs/tree/master/cpp-example/rust-part).
The Rust part is compiled as shared library and linked into executable
described by [CMakeLists.txt](https://github.com/Dushistov/flapigen-rs/tree/master/cpp-example/cpp-part/CMakeLists.txt).
The cpp-part is the main part so its build system (cmake) invokes `cargo` to build the Rust part.

## Building

It is a normal CMake project, so you can build it as an ordinary CMake project.
By default it requires C++11 and boost, but if your compiler is modern enough
you can use C++17 and then you don't need boost at all.

Just delete all mentions of boost here:

```rust,no_run,noplaypen
// build.rs
{{#include ../../cpp-example/rust-part/build.rs:cpp_config}}
```

## The main functionality

This project demonstrates how to export Rust in the form of a class to C++.

Rust code:

```rust,no_run,noplaypen
// src/lib.rs
{{#include ../../cpp-example/rust-part/src/lib.rs:rust_class}}
```

Described as class:

```rust,no_run,noplaypen
// src/cpp_glue.rs.in
{{#include ../../cpp-example/rust-part/src/cpp_glue.rs.in:basic_cpp_class}}
```

Usage from C++:

```c++,no_run,noplaypen
// main.cpp
{{#include ../../cpp-example/cpp-part/main.cpp:call_rust}}
```

## Borrowed foreign classes

For a regular `foreign_class!`, Rust `&Foo` arguments and return values use
`FooRef` by value in C++. A `Foo` or `const Foo&` converts implicitly to
`FooRef`, so either an owning object or an existing borrowed view can be passed
to a generated method. For example, the [C++ test fixture](https://github.com/Dushistov/flapigen-rs/blob/master/cpp_tests/c%2B%2B/main.cpp)
uses `TestReferences` and `Foo`:

```cpp
TestReferences source{1, "source"};
TestReferences target{2, "target"};
FooRef view = source.get_foo_ref();
target.update_foo(view);
const Foo owned{3, "owned"};
target.update_foo(owned);
```

Copying a `FooRef` copies only its pointer. Keep the Rust object alive while
using the view. Rust `&mut Foo` arguments still take `Foo&` in C++; a returned
`&mut Foo` currently becomes a read-only `FooRef`.

## A plain C++ class

By default, flapigen generates two C++ variants for a `foreign_class!`: an
owning class and a non-owning reference class. If you don't want to deal with two variants and
the need to convert one into the other in various cases, `#[derive(PlainClass)]` generates just
one C++ class. By default, returning a Rust reference to that type
(`&ScoreAdjustment` or `&mut ScoreAdjustment`) gives a compile error because
there is no C++ reference wrapper. Return an owned value instead, or define an
explicit outgoing `foreign_typemap!` for a custom borrowed C++ view. The C++
caller must keep the Rust referent alive while using such a view. The single
class can also be forward-declared as
`class ScoreAdjustment;` in a handwritten C++ header.

The Rust type and its binding are:

```rust,no_run,noplaypen
{{#include ../../cpp-example/rust-part/src/lib.rs:plain_class_rust}}
```

```rust,no_run,noplaypen
{{#include ../../cpp-example/rust-part/src/cpp_glue.rs.in:plain_class_binding}}
```

The C++ helper header can now declare a function using the generated class:

```cpp,no_run,noplaypen
{{#include ../../cpp-example/cpp-part/score_report.hpp:plain_class_forward_declaration}}
```

The C++ implementation includes `ScoreAdjustment.hpp` before calling its
method:

```cpp,no_run,noplaypen
{{#include ../../cpp-example/cpp-part/main.cpp:plain_class_cpp_implementation}}
```

The executable uses the helper like this:

```cpp,no_run,noplaypen
{{#include ../../cpp-example/cpp-part/main.cpp:plain_class_cpp_use}}
```
