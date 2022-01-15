# foreign_typemap!

`flapigen` works with a [graph](https://en.wikipedia.org/wiki/Graph_(discrete_mathematics)) where
vertices are types and edges are conversion rules between types. To extend or rewrite some rules
you can use the `foreign_typemap!` macro. **WARNING** this syntax is experimental and subject to change.

Let's take a look at an example:

```rust,no_run,noplaypen
{{#include ../../jni_tests/src/chrono-include.rs:foreign_typemap_chrono_example}}
```

The `(r_type)` part of the rule describes a conversion from any Rust type to a Rust type that
can cross the FFI (Foreign Function Interface) border.
For example, in the case of connection Rust and C++, types that can cross the FFI border are nearly all primitive types,
plus types marked with `repr(C)` and some others. After conversion to the type that can cross the FFI border,
you sometimes want to describe how this type should be converted to "foreign" type. This is done with the
`(f_type)` part of the rule. Returning to our example, first we describe how to convert `DateTime<Utc>` (Rust type) to
`jlong` (Rust type). Then we describe how to convert `long` (Java type) to `java.util.Date` (Java type).

There is one nuance. As you can see the Java type is called `/*chrono*/java.util.Date` instead of
just `java.util.Date`. This is done because you don't want two edges to one vertex in our graph
mentioned above. There is already a rule for `std::time::SystemTime` (Rust type) to `java.util.Date` (Java type),
and it is better not have two rules (edges) that lead to the same vertex `java.util.Date`,
so we create new unique vertex `/*chrono*/java.util.Date` != `java.util.Date`.

Almost all parts of `foreign_typemap!` are optional.
In the example below we define that `jlong` (Rust type) is corresponding to `long` (Java type),
and there is no need to convert anything to get `long` from `jlong` and vise versa.

```rust,no_run,noplaypen
{{#include ../../macroslib/src/java_jni/jni-include.rs:foreign_typemap_define_jlong}}
```

It is also possible to use `foreign_typemap!` for generic type conversion.
In the example below we define the rule to convert any Java type that looks like `X []`
to `Vec<T>` if type `T` implements the traits `SwigForeignClass + Clone`.

```rust,no_run,noplaypen
{{#include ../../macroslib/src/java_jni/jni-include.rs:foreign_typemap_generic_example}}
```
