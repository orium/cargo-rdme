# Release notes

## 0.7.0

* Automatically bump the heading level in the injected rustdoc based on the README current heading level.

## 0.6.0

* Added option for intralinks to be stripped.
* Base url and version in intralinks urls can now be configured.
* Strip intralinks when we cannot generate a correct link.
* Fix warning and error output when not writing to a tty.

## 0.5.0

* Fail on `--check` if there were warnings.
  * Add command line option to not fail on warnigs.

## 0.4.0

* Add support for intralinks!
  * Intralinks are not converted to links to docs.rs in your README file.
* Add support for workspaces.
* Add option to override the README file path (command line flag and configuration file).
* Support code blocks with more than three backticks.
* Use nice color output for errors and warnings.

## 0.3.0

* Add `rust` markdown tag to rust code blocks.
* Avoid overwriting README files with uncommitted changes.

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
