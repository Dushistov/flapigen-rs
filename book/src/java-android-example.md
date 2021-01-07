# Java/Android

[This example](https://github.com/Dushistov/flapigen-rs/tree/master/android-example) shows off how to use rust to build a native library from android
and use it through an automatically generated JNI wrapper.

[Android Studio](https://developer.android.com/studio) can be used to work with Rust via [Rust plugin](https://intellij-rust.github.io/).
So it is not bad idea to integrate invocation of cargo into gradle,
thus you can build and run android application with Rust inside as ordinary Java/Kotlin application.

## Project Structure

The file `build.rs` defines how flapigen generates wrapper code:

```rust,no_run,noplaypen
// build.rs
{{#include ../../android-example/build.rs:config}}
```

The file `src/lib.rs` contains real code that will be invoked from Java:
```rust,no_run,noplaypen
// src/lib.rs
{{#include ../../android-example/src/lib.rs:rust_code}}
```

And the file `src/java_glue.rs.in` contains description for flapigen how export this API to Java:

```rust,no_run,noplaypen
// src/java_glue.rs.in
{{#include ../../android-example/src/java_glue.rs.in:api}}
```

Then the `app/build.gradle` contains rule how to invokes `cargo` to build shared library from Rust code,
and then build it into apk:

```groovy,no_run,noplaypen
// app/build.gradle
{{#include ../../android-example/app/build.gradle:cargo}}
```

## Building

To build the demo, you will need the latest version of Cargo, Android NDK and install proper Rust toolchains:

``` shell
rustup target add arm-linux-androideabi
rustup target add aarch64-linux-android
```

To link the result into shared library you need add path to proper clang binary into `PATH`
environment variable or change path to linker here:

```toml
{{#include ../../android-example/.cargo/config}}
```

## Invocation

Gradle will take care of building and deploying the Rust sources. Thus, to build
the project in release mode, simply call `./gradlew androidRelease`.

To build only the rust libraries for a specific target, call cargo as usual, e.g.
`cargo build --target arm-linux-androideabi`.

## Testing

It is possible to run Rust unit tests on Android phone via `run-on-android.sh` script mentioned in `.cargo/config`,
also there is instrumentation unit test on Java that invoke Rust code.
