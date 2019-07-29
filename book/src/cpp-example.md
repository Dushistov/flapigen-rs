# C++

To run cpp-example from [here](https://github.com/Dushistov/rust_swig/tree/master/cpp_example) you need [boost](https://www.boost.org/), [cmake](https://cmake.org/) and C++11 compatible compiler.

After that you should have no problem to call this Rust code:

```rust,no_run,noplaypen
// src/lib.rs
{{#include ../../cpp-example/rust-part/src/lib.rs:rust_class}}
```

from C++:

```c++,no_run,noplaypen
// main.cpp
{{#include ../../cpp-example/cpp-part/main.cpp:call_rust}}
```
