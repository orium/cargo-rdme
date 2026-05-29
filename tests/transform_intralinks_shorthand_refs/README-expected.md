# Shorthand Reference Safety Test

<!-- cargo-rdme start -->

The shorthand pass must not corrupt reference links or reference definitions.

Bare shorthand is converted to a link: [`BareStruct`](https://docs.rs/integration_test/latest/integration_test/struct.BareStruct.html).

Shortcut reference with an explicit definition is left untouched: [`DefStruct`].

Full reference link is left untouched: [`RefStruct`][rs].

[`DefStruct`]: https://docs.rs/integration_test/latest/integration_test/struct.DefStruct.html
[rs]: https://docs.rs/integration_test/latest/integration_test/struct.RefStruct.html

<!-- cargo-rdme end -->
