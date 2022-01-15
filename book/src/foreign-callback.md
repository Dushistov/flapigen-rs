# foreign_callback!

You can also use a `trait` to describe a callback from Rust to Java/C++:

```rust,no_run,noplaypen
{{#include ../../jni_tests/src/java_glue.rs.in:trait_as_callback}}
```

As a result of `flapigen` processing `foreign_callback!` it generates an **interface** for Java and
an abstract **class** for C++, so you can implement methods in Java/C++ and pass a pointer/reference to Rust,
and for Rust it would be represented as a **trait** implementation.
