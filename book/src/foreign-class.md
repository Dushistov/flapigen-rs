# foreign_class!

`foreign_class!` is the way to describe an entity that will be visible for the "foreign language" as a class.

The basic example is:

```rust,no_run,noplaypen
{{#include ../../cpp-example/rust-part/src/cpp_glue.rs.in}}
```

Here `Foo` may be **struct** or **enum** or a more complex type (see below).
The **self_type** can be omitted if there are no **constructor**s and all **fn** do not accept **self**.

## self_type

The **self_type** is used as a "neat" way to call methods of types inside "smart pointers".
For example sometimes you can not use your **struct** or **enum** explicitly, because of the semantic
of the "foreign language" does not allow it, then you can write:

```rust,no_run,noplaypen
{{#include ../../jni_tests/src/java_glue.rs.in:arc_mutex_usage}}
```

And `flapigen` will be instructed to generate code to call each method listed above
with `lock().unwrap().method` syntax.

Also maybe you want export a `trait` implementation as `foregin_class`.
The `flapigen` cannot work with `Box<Trait>`, because it requires unstable/unsafe APIs to
convert the boxed trait into something that can be transferred over the FFI border,
but it can work with `Box<Box<Trait>>`, so via **self_type** you can represent a boxed trait as class:

```rust,no_run,noplaypen
{{#include ../../cpp_tests/src/cpp_glue.rs.in:boxed_trait}}
```

Also it should be noted that `flapigen` generates code that uses "Universal Function Call Syntax",
so methods can be fake methods and declared outside of **impl** blocks:

```rust,no_run,noplaypen
{{#include ../../cpp_tests/src/cpp_glue.rs.in:method_not_method}}
```

## Access modifiers

By default, methods in generated class are public.
You can change access level via protected and private keywords.

```rust,no_run,noplaypen
{{#include ../../macroslib/tests/expectations/access.rs}}
```

## Inline methods

It is also sometimes convenient to write short methods just inside `foreign_class!`, for example to add a way to access
fields of a **struct** or to export a result of a Rust macro call:

```rust,no_run,noplaypen
{{#include ../../cpp_tests/src/cpp_glue.rs.in:inline_method}}
```

To reference `self` in inline methods you should use the `this` variable, because the inline method
is not the real one, it is just code block that gets included into the generated code:

```rust,no_run,noplaypen
{{#include ../../cpp_tests/src/cpp_glue.rs.in:inline_method_self}}
```

## Methods aliases

Also you can create an alias for a function name:

```rust,no_run,noplaypen
{{#include ../../jni_tests/src/java_glue.rs.in:alias_usage}}
```

So it would be called by path with **fn** in Rust code,
and you can call it via name with **alias** in foreign language.

This may be useful for example if you want to name functions in Java in camel case style,
while you want to use snake case style in Rust.

## Constructors

Constructors are Rust methods that mapped to constructors in terms of the "foreign" language.
Also constructors, more precisely, the return type of constructors can be used to
ask `flapigen` to simplify calls of methods, see the self_type section for more details.
Sometimes you need a constructor, but you don't want allow the user to construct objects,
then you can use an empty constructor:

```rust,no_run,noplaypen
{{#include ../../jni_tests/src/java_glue.rs.in:constructor_empty}}
```

## foreigner_code

flapigen also supports bypassing code generation:

```rust,no_run,noplaypen
{{#include ../../jni_tests/src/java_glue.rs.in:foreign_code_usage}}
```

After that you can implement the Java_com_example_TestPathAndResult_do_1testHandArrayReturn
function yourself, useful for when flapigen cannot handle something automatically,
or you want to do something special.

## Doc comments

You can also add comments to generated code with Rust doc comments:

```rust,no_run,noplaypen
{{#include ../../jni_tests/src/java_glue.rs.in:doc_comments_usage}}
```

## Derives

You can use "derive" syntax on the declaration of classes, in a similar way to the usage on "Rust" structs:

```rust,no_run,noplaypen
{{#include ../../jni_tests/src/java_glue.rs.in:derive_usage}}
```

Usage of derives changes generated code in various ways.
For example, you can use `Clone,Copy` to force the generation of a copy constructor
and `operator=` in C++ case.
You can also use `camelCaseAliases` to change names of all methods to camel case.
