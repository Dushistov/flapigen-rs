# Foreign Language API Description

`rust_swig` provides several methods to describe how your Rust code can be used
from "foreign" programming language. The main build block is `foreign_class`.
This is the way how describe entity that will be visible for "foreign language" as a class.
Also it is possible to export C-like enums via `foreign_enum`, plus it is possible to describe the
way how to pass "callback" into your Rust code from foreign language.
