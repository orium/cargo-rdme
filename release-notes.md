# Release notes

## 0.2.0

* Rust code blocks starting with `#` are now omitted, just like in `rustdoc`.
* Support default bin entrypoint paths.
* Fix command line parsing bug when cargo-rdme was invoked as a cargo subcommand.

## 0.1.0

* Initial version.
* Basic README syncronization.
* Command line flags to control line terminator and entrypoint.
* Support for configuration file.
* Allow `cargo rdme` to be used easily integrated with CIs with the `--check` command line flag.

## 0.0.0

* Initial release to reserve the crate's name in crates.io.
