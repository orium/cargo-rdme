[![Build Status](https://github.com/orium/cargo-rdme/workflows/CI/badge.svg)](https://github.com/orium/cargo-rdme/actions?query=workflow%3ACI)
[![Code Coverage](https://codecov.io/gh/orium/cargo-rdme/branch/master/graph/badge.svg)](https://codecov.io/gh/orium/cargo-rdme)
[![Dependency status](https://deps.rs/repo/github/orium/cargo-rdme/status.svg)](https://deps.rs/repo/github/orium/cargo-rdme)
[![crates.io](https://img.shields.io/crates/v/cargo-rdme.svg)](https://crates.io/crates/cargo-rdme)
[![Downloads](https://img.shields.io/crates/d/cargo-rdme.svg)](https://crates.io/crates/cargo-rdme)
[![Github stars](https://img.shields.io/github/stars/orium/cargo-rdme.svg?logo=github)](https://github.com/orium/cargo-rdme/stargazers)
[![License](https://img.shields.io/crates/l/cargo-rdme.svg)](./LICENSE.md)

<!-- cargo-rdme start -->

# Cargo rdme

Cargo command to create your README from your crate’s documentation.

## Usage

Cargo rdme will insert your crate’s documentation in your README file.  To control where the
documentation will be inserted you need to insert a marker: `<!-- cargo-rdme -->`.  For example,
you can start your README with some glorious badges and follow up with the rustdoc
documentation:

```markdown
[![Build Status](https://example.org/badge.svg)](https://example.org/link-to-ci)

<!-- cargo-rdme -->
```

After running `cargo rdme` you will find your README to be something like:

```markdown
[![Build Status](https://example.org/badge.svg)](https://example.org/link-to-ci)

<!-- cargo-rdme start -->

<WHATEVER-YOUR-CRATES-DOC-IS>

<!-- cargo-rdme end -->
```

Whenever change your crate’s documentation you just need to run `cargo rdme` to update your
README file.

## Automatic transformations

The documentation of your crate doesn’t always map directly to a good README.  For example,
rust code blocks can have hidden lines.  Those should not be shown in the README file.

This section covers the transformation cargo rdme automatically apply to generate a better
README.

### Rust code block

Rust code block are transformed in two ways by cargo rdme:

1. Rust code blocks with lines starting with `#` will be omitted, just like in `rustdoc`.
2. Rust code blocks get annotated with the `rust` markdown tag so it gets proper syntax
   highlighting.  We also remove tags that only concern `rustdoc` such as `should_panic`.

In the table below you can see an example of these modification.  The code block now is
tagged with `rust` and hidden lines were removed:

<table border="1">
<col span="1" width="40%">
<col span="1" width="40%">
</colgroup>
<tr>
<th><center>Crate’s rustdoc</center></th>
<th><center>README.md</center></th>
<tr>
<tr>
<td>

```rust
//! To check if a number is prime do:
//!
//! ```
//! # fn main() {
//! for i in 2.. {
//!     if is_prime(i) {
//!         println!("{}", i);
//!     }
//! }
//! # }
//! ```
```

</td>
<td>

````markdown
To check if a number is prime do:

```rust
for i in 2.. {
    if is_prime(i) {
        println!("{}", i);
    }
}
```
````

</td>
</tr>
</table>

### Intralinks

Rust documentation can contain [links to items defined in the crate](https://doc.rust-lang.org/stable/rustdoc/linking-to-items-by-name.html).
This links would not make sense in your README file, so cargo rdme automatically generate
links to [docs.rs](https://docs.rs) for these intralinks.

Currently we only support links of the form `[⋯](crate::⋯)`, so be sure to use that format.
Links to the standard library are also supported, and they must be of the form
`[⋯](::<crate>::⋯)`, where `<crate>` is a crate that is part of the standard library, such as
`std`, `core`, or `alloc`.

Take a look at the example below:

<table border="1">
<col span="1" width="40%">
<col span="1" width="40%">
</colgroup>
<tr>
<th><center>Crate’s rustdoc</center></th>
<th><center>README.md</center></th>
<tr>
<tr>
<td>

```rust
//! To check if a number is prime use
//! [`is_prime`](crate::is_prime).
```

</td>
<td>

```markdown
To check if a number is prime use
[`is_prime`](https://docs.rs/prime/latest/prime/fn.is_prime.html)
```

</td>
</tr>
</table>

Note that there is some limitations in intralink support.  This is a complex feature: cargo rdme
needs to do some work to be able to create the link to docs.rs.  This is because the link
includes the kind of item the intralink points to, in the case of `is_prime` we need to discover
that is a function to generate a link that ends in `fn.is_prime.html`.  Therefore, intralink
support should be considered "best effort" (for instance, don’t expect items generated by macros
to be resolved).  If cargo rdme is unable to generate the link it will still generate the README
file, but a warning will be emitted.

## Configuration file

If the default behavior of `cargo rdme` is not appropriate for your project you can crate a
configuration file `.cargo-rdme.toml` in the root of your project.  This is how that
configuration file can look like:

```toml
# Override the README file path.  When this is not set cargo rdme will use the file path defined
# in the project’s `Cargo.toml`.
readme-path = "MY-README.md"

# What line terminator to use when updating the README file.  This can be "lf" or "crlf".
line-terminator = "lf"

# The default entrypoint will be `src/lib.rs`.  You can change that in the `entrypoint` table.
[entrypoint]
# The entrypoint type can be "lib" or "bin".
type = "bin"
# When you set type to "bin" the entrypoint default to `src/main.rs`.  If you have binary targets
# specified in your cargo manifest you can select them by name with `bin-name`.
bin-name = "my-bin-name"
```

These setting can be overridden with command line flags.  Run `cargo rdme --help` for more
information.

## Integration with CI

To verify that your README is up to date with your crate’s documentation you can run
`cargo rdme --check`.  The exit code will be `0` if the README is up to date, or `2` if it’s
not.

<!-- cargo-rdme end -->
