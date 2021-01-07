# foreign_typemap!

`flapigen` works with [graph](https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)) where
vertices are types and edges are conversation rules between types. To extend or rewrite some rules
you can use `foreign_typemap!` macro. **WARNING** syntax is experimental and subject to change.

Let's take a look at an example:

```rust,no_run,noplaypen
{{#include ../../jni_tests/src/chrono-include.rs:foreign_typemap_chrono_example}}
```

The `(r_type)` part of rule is described conversation from any Rust type to Rust type that
can cross FFI (Foreign Function Interface) border.
For example, in case of connection Rust and C++, types that can cross border are almost all primitive types,
plus types marked with `repr(C)` and some others. After conversation to type that can cross FFI border,
you sometimes want describe how this type should be converted to "foreign" type. This is done with
`(f_type)` part of rule. Returning to our example at first we describe how to convert `DateTime<Utc>` (Rust type) to
`jlong` (Rust type). Then we describe how to convert `long` (Java type) to `java.util.Date` (Java type).

There is one nuance. As you can see Java type is called `/*chrono*/java.util.Date` instead of
just `java.util.Date`. This is done because of you don't want two edges to one vertex in our graph
mentioned above. There is already rule for `std::time::SystemTime` (Rust type) to `java.util.Date` (Java type),
and it is better not have two rules (edges) that lead to the same vertex `java.util.Date`,
so we create new unique vertex `/*chrono*/java.util.Date` != `java.util.Date`.

Almost all parts of `foreign_typemap!` are optional.
In the example bellow we define that `jlong` (Rust type) is corresponding to `long` (Java type),
and there is no need to convert anything to get `long` from `jlong` and vise versa.
```rust,no_run,noplaypen
{{#include ../../macroslib/src/java_jni/jni-include.rs:foreign_typemap_define_jlong}}
```

Also it is possible to use `foreign_typemap!` for generic type conversation.
In the example bellow we define the rule to convert any type Java type that looks like `X []`
to `Vec<T>` if for type `T` there are traits `SwigForeignClass + Clone`.
```rust,no_run,noplaypen
{{#include ../../macroslib/src/java_jni/jni-include.rs:foreign_typemap_generic_example}}
```
