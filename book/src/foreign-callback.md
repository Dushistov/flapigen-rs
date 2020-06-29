# foreign_callback!

Also you can use `trait` to describe callback from Rust to Java/C++:

```rust,no_run,noplaypen
{{#include ../../jni_tests/src/java_glue.rs.in:trait_as_callback}}
```

As result of `flapigen` processing `foreign_callback!` it generates **interface** for Java and
abstract **class** for C++, so you can implements methods in Java/C++ and pass pointer/reference to Rust,
and for Rust it would be represented as **trait** implementation.

