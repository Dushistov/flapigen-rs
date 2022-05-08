# Foreign Language API Description

`flapigen` provides several methods to describe how your Rust code can be used
from a "foreign" programming language. The main building block is [foreign_class!](./foreign-class.md).
This is the way to describe entities that will be visible for the "foreign language" as classes.
It is also possible to export C-like enums via [foreign_enum!](./foreign-enum.md), plus it is possible to describe the
way to pass a "callback" into your Rust code from the foreign language via [foreign_callback!](./foreign-callback.md).
And of course it is possible to extend or rewrite existing type conversions rules via [foreign_typemap!](./foreign-typemap.md)
