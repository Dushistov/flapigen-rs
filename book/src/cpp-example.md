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
{{#include ../../cpp-example/rust-part/src/cpp_glue.rs.in}}
```

Usage from C++:

```c++,no_run,noplaypen
// main.cpp
{{#include ../../cpp-example/cpp-part/main.cpp:call_rust}}
```
