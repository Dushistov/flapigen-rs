# Generated-code expectations

To add a test, put its `<name>.rs` fixture in this directory and add `<name>`
to `tests.list`. Create `<name>.cpp` or `<name>.java` for foreign code; add
`<name>.cpp_rs` or `<name>.java_rs` if generated Rust also needs checking.
Rust output uses the generated filename `test.rs` in selectors.

Add an `@@expect` section with an empty body, then bless the focused test:

```text
@@expect {"file":"Foo.hpp","kind":"item","name":"FooWrapper<OWN_DATA>::f6","form":"definition"}

@@end
```

Each header needs `file` (the generated filename, without a directory) and
`kind`:

- `"file"` checks the whole file.
- `"item"` checks a named item. Set `name` to its name and `form` to
  `"declaration"` or `"definition"`.
- `"between"` checks text between `before` and `after` strings, excluding the
  strings themselves. Omit an empty `before` or `after` to use the start or end
  of the file; at least one boundary is required. By default, the first
  `after` following `before` ends the match. Set `"greedy_match":true` to use
  the last `after` and capture the longest span from that `before` boundary.
  Multiple `before` matches are still ambiguous. Use a complete nearby line
  in `before` so a failed selector is easy to locate in generated code.

Run this command to create or update the expected body:

```sh
UPDATE_EXPECT=1 cargo test -p flapigen --test test_expectations test_expectation_my_fixture
```

Replace `my_fixture` with the fixture name. Review the updated expectation,
then rerun without `UPDATE_EXPECT`. If a selector no longer identifies one
location, edit its header before blessing.
