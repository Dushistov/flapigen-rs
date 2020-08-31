# Foreign Language API Description

`flapigen` provides several methods to describe how your Rust code can be used
from "foreign" programming language. The main build block is [foreign_class!](./foreign-class.md).
This is the way how describe entity that will be visible for "foreign language" as a class.
Also it is possible to export C-like enums via [foreign_enum!](./foreign-enum.md), plus it is possible to describe the
way how to pass "callback" into your Rust code from foreign language via [foreign_callback!](./foreign-callback.md).
And of course it is possible to extend or rewrite existing type conversations rules via [foreign_typemap!](./foreign-typemap.md)
