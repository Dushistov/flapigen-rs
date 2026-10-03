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

## A plain C++ class

By default, flapigen generates two C++ variants for a `foreign_class!`: an
owning class and a non-owning reference class. If you don't want to deal with two variants and
the need to convert one into the other in various cases, `#[derive(PlainClass)]` generates just
one C++ class. The tradeoff is that you cannot return a Rust reference to that
type (`&ScoreAdjustment` or `&mut ScoreAdjustment`) to C++; return an owned
value instead. The single class can also be forward-declared as
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
