# Android Example

This example shows off how to use rust to build a native library from android
and use it through an automatically generated JNI wrapper.

## Project Structure

The file `build.rs` defines how rust_swig generates wrapper code. It
automatically finds a suitable `jni.h` and generates a rust source file against
it. Then, the script recursively looks for files ending in `.rs.in` in the
source directory and uses rust_swig to generate a JNI wrapper both in Rust and
in Java.

This build script is intended to be launched from gradle through e.g. `./gradlew
aR`. The gradle build files contain definitions on how to build the Rust
libraries and where to find them for inclusion in the apk.

## Building

### Prerequisites
To build the demo, you will need the latest version of Cargo and at least rustc
1.16. You will also need to add support for android targets:

``` shell
rustup target add arm-linux-androideabi
rustup target add aarch64-linux-android
rustup target add i686-linux-android
rustup target add x86_64-linux-android
```

To link the libraries, you will need the android NDK and generate standalone
toolchains for each target (edit the install dir as required):

``` shell
# ARM
$ANDROID_NDK/build/tools/make-standalone-toolchain.sh --platform="android-27" --toolchain=arm-linux-androideabi-4.9 --install-dir=$ANDROID_TOOLCHAINS/android-27-arm-linux-androideabi-4.9  --arch=arm
$ANDROID_NDK/build/tools/make-standalone-toolchain.sh --platform="android-27" --toolchain=aarch64-linux-android-4.9 --install-dir=$ANDROID_TOOLCHAINS/android-27-aarch64-linux-android-4.9  --arch=aarch64

# x86
$ANDROID_NDK/build/tools/make-standalone-toolchain.sh --platform="android-27" --toolchain=x86-4.9 --install-dir=$ANDROID_TOOLCHAINS/android-27-x86-4.9  --arch=x86
$ANDROID_NDK/build/tools/make-standalone-toolchain.sh --platform="android-27" --toolchain=x86_64-4.9 --install-dir=$ANDROID_TOOLCHAINS/android-27-x86_64-4.9  --arch=x86_64
```

Then edit `.cargo/config` to point to the toolchains you just generated.

### Invocation

Gradle will take care of building and deploying the Rust sources. Thus, to build
the project in release mode, simply call `./gradlew androidRelease`.

To build only the rust libraries for a specific target, call cargo as usual ,
but specify the environment variable `ANDROID_APPLICATION_ID`(e.g.
`ANDROID_APPLICATION_ID="net.akaame.myapplication" cargo build --target
arm-linux-androideabi`).
