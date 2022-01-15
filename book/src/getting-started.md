In the first step you should choose "main" crate that would be the bridge between Rust and other language.
It should have `crate-type` `cdylib` or `staticlib`, see [suitable Rust book's section](https://doc.rust-lang.org/reference/linkage.html) for a detailed description.

For example:

```toml,no_run,noplaypen
{{#include ../../cpp-example/rust-part/Cargo.toml:cargo_lib_section}}
```

Then you should create `build.rs` inside this crate.
For example:

```rust,no_run,noplaypen
//build.rs
{{#include ../../cpp-example/rust-part/build.rs}}
```

Here you instruct `flapigen` to generate C++/Rust code from "src/cpp_glue.rs.in" as **input**:

```rust,no_run,noplaypen
{{#include ../../cpp-example/rust-part/build.rs:rust_input}}
```

and we specify our **output**,
Rust file:

```rust,no_run,noplaypen
{{#include ../../cpp-example/rust-part/build.rs:rust_output}}
```

directory for C++ files:

```rust,no_run,noplaypen
{{#include ../../cpp-example/rust-part/build.rs:cpp_output}}
```

You can find a detailed description of code generation in the [suitable Cargo book section](https://doc.rust-lang.org/cargo/reference/build-scripts.html#case-study-code-generation).

Then you should create a file for the "foreign" language API description.
For example:

```rust,no_run,noplaypen
{{#include ../../cpp-example/rust-part/src/cpp_glue.rs.in}}
```

And connect the generated code with your crate exactly how as in the [Cargo book](https://doc.rust-lang.org/cargo/reference/build-scripts.html#case-study-code-generation) :

```rust,no_run,noplaypen
// src/lib.rs
{{#include ../../cpp-example/rust-part/src/lib.rs:connect}}
```

```rust,no_run,noplaypen
// src/cpp_glue.rs
{{#include ../../cpp-example/rust-part/src/cpp_glue.rs:connect}}
```

Do not forget to add `flapigen` as a dependency into `[build-dependencies]` section of your crate's `Cargo.toml` file and you are ready to go.
