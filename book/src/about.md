`rust_swig` is tool for connecting programs or libraries written in Rust with other languages.
Currently implemented support for `C++` and `Java`, but you can write support
for any language of your choice.
It is designed to run by cargo during via cargo's [build script](https://doc.rust-lang.org/cargo/reference/build-scripts.html) mechanism.

At first you choose what part of your Rust API will be "exported" to other programming language,
and how. At the second step in cargo's build script you describe where get API description and where
put resulted Rust and other language code that generated after processing API description.

Cooperation with other programming languages (in general) is done via C API,
so generated Rust's code is wrapper around you code to provide C API.
Generated code in other programming language is the way to make your API
as easy as possible to use in other language.
