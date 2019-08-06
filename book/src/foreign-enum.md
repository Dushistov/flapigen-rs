# foreign_enum!

With usage of `foreign_enum!` macro you can "export" `enum` (`C` like enum) to foreign language:

```rust,no_run,noplaypen
{{#include ../../jni_tests/src/java_glue.rs.in:foreign_enum_usage}}
```

This allow you can use it as input or output types for `foreign_class!` methods.


