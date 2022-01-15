# foreign_enum!

Through the usage of the `foreign_enum!` macro you can "export" `enum`s (`C` like enum) to a foreign language:

```rust,no_run,noplaypen
{{#include ../../jni_tests/src/java_glue.rs.in:foreign_enum_usage}}
```

This allows you to use it as input or output types for `foreign_class!` methods.
