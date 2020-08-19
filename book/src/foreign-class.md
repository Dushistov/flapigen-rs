# foreign_class!

`foreign_class!` is the way how describe entity that will be visible for "foreign language" as a class.

The basic example is:

```rust,no_run,noplaypen
{{#include ../../cpp-example/rust-part/src/cpp_glue.rs.in}}
```

Here `Foo` may be **struct** or **enum** or more complex type (see bellow).
The **self_type** can be omitted if there are no **constructor**s and all **fn** do not accept **self**.

## self_type

The **self_type** is used as "neat" way to call methods of types inside "smart pointers".
For example sometimes you can not use your **struct** or **enum** explicitly, because of semantic
of "foreign language" is not allow it, then you can write:


```rust,no_run,noplaypen
{{#include ../../jni_tests/src/java_glue.rs.in:arc_mutex_usage}}
```

And `flapigen` will be instructed to generate code to call each method listed above 
with `lock().unwrap().method` syntax.


Also may be you want export `trait` implementation as `foregin_class`.
The `flapigen` can not work with `Box<Trait>`, because of it requires unstable/unsafe API to
convert boxed trait to something that can be transferred via FFI border,
but it can work with `Box<Box<Trait>>`, so via **self_type** you can represent boxed trait as class:

```rust,no_run,noplaypen
{{#include ../../cpp_tests/src/cpp_glue.rs.in:boxed_trait}}
```

Also that should be noted that `flapigen` generates code that uses "Universal Function Call Syntax",
so method can be not real methods and declared outside of **impl** blocks:

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

It is also sometimes convenient to write short methods just inside `foreign_class!`, for example to add way to access
fields of **struct** or export result of Rust's macros application:

```rust,no_run,noplaypen
{{#include ../../cpp_tests/src/cpp_glue.rs.in:inline_method}}
```
To reference `self` in inline methods you should use `this` name, because of inline method
is not the real one, it is just code block that included into generated code:
```rust,no_run,noplaypen
{{#include ../../cpp_tests/src/cpp_glue.rs.in:inline_method_self}}
```

## Methods aliases

Also you can create alias for function name:

```rust,no_run,noplaypen
{{#include ../../jni_tests/src/java_glue.rs.in:alias_usage}}
```

So it would be called by path after **fn** in Rust code,
and you can call it via name after **alias** in foreign language.

May be useful for example if you want name functions in Java in camel case style,
while want in Rust use snake case style.


## foreigner_code

Also flapigen support bypassing of code generation:

```rust,no_run,noplaypen
{{#include ../../jni_tests/src/java_glue.rs.in:foreign_code_usage}}
```

after that you can implement Java_com_example_TestPathAndResult_do_1testHandArrayReturn
function by your self, usefull when flapigen can not handle something automaticaly,
or you want something special.

## Doc comments

Also you can add comments to generated code with Rust's doc comments:

```rust,no_run,noplaypen
{{#include ../../jni_tests/src/java_glue.rs.in:doc_comments_usage}}
```
