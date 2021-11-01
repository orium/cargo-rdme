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

### Automatic transformations

Cargo rdme will apply some automatic transformations to your documentation when generating the README file:

1. Rust code blocks starting with `#` will be omitted, just like in `rustdoc`.

## Config file

If the default behavior of `cargo rdme` is not appropriate for your project you can crate a
configuration file `.cargo-rdme.toml` in the root of your project.  This is how that
configuration file can look like:

```toml
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
