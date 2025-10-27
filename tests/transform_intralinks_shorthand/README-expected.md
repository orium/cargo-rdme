# Shorthand Link Test

<!-- cargo-rdme start -->

## Shorthand Link Tests

Basic shorthand links: [`my_function()`](https://docs.rs/integration_test/latest/integration_test/fn.my_function.html), [`MyStruct`](https://docs.rs/integration_test/latest/integration_test/struct.MyStruct.html), and [`MY_CONST`](https://docs.rs/integration_test/latest/integration_test/const.MY_CONST.html).

Shorthand links in a list:
- [`first_function()`](https://docs.rs/integration_test/latest/integration_test/fn.first_function.html)
- [`second_function()`](https://docs.rs/integration_test/latest/integration_test/fn.second_function.html)

Edge case: shorthand link followed by space and parenthesis (the bug we fixed):
[`important_function()`](https://docs.rs/integration_test/latest/integration_test/fn.important_function.html) (this is very fast).

Multiple on one line: [`foo()`](https://docs.rs/integration_test/latest/integration_test/fn.foo.html) and [`bar()`](https://docs.rs/integration_test/latest/integration_test/fn.bar.html) work together.

Already converted (should not double-convert): [`already_linked()`](https://example.com).

Not a shorthand link (no backticks): [regular link](https://example.com).

Nested in text: The [`nested_func()`](https://docs.rs/integration_test/latest/integration_test/fn.nested_func.html) is useful for processing.

<!-- cargo-rdme end -->
