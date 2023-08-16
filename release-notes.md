# Release notes

## 2.0.0-pre

1. Use rustdoc JSON for intralink resolution. This fixes a bunch of limitation we had with the previous `syn`-based
   engine:
   1. Support relative paths from the crate root (`self::`, bare names, multi-level module-relative).
   2. Support all item kinds, including struct/union fields, enum variants, trait items, type aliases, statics, macros,
      proc-macros, and primitive types (#10, #169).
   3. Support explicit kind disambiguators (`struct@`, `fn@`, `primitive@`, ...) and suffix forms (`Foo()`, `Foo!`)
      (#1, #186).
   4. Support intralinks to items in any external crate, including regular dependencies and sibling crates within a
      workspace (#152, #153).
   5. Support intralinks through re-exports, including local `pub use` and stdlib re-exports like `std::vec::Vec` (#5).
   6. Support impl items defined in a module other than the type's defining module (#170).
   7. Support rustdoc's shortcut link form for intralinks, in backticked (`` [`Foo`] ``), path (`[foo::Bar]`), and
      bare-identifier (`[Foo]`) variants (#7).
   8. Macro-generated items, which the previous walker could not see, are now supported.
2. Add `--all-features`, `--features`, and `--no-default-features` flags (and matching `[intralinks]` config keys) to
   control how rustdoc builds the crate.

## 1.5.1

* Use `gix` instead of `git2` to avoid overwriting a modified `README.md`.
* Update dependencies.

## 1.5.0

* Added the `--manifest-path` command line option.

## 1.4.9

* Relicensed project under the MIT license.
* Update dependencies.

## 1.4.8

* Use cargo styles for cli help output.
* Update dependencies.

## 1.4.7

* Another attempt at fixing github build action.

## 1.4.6

* Update dependencies.
* Fix github build action: we should be back to publishing binaries.

## 1.4.5

* Update dependencies.

## 1.4.4

* Fix selection of packages in workspaces when there's a package name conflict with a dependency. 
* Update dependencies.

## 1.4.3

* Update dependencies.

## 1.4.2

* It turns out we might need the network to get the project metadata.  See release notes of version 1.4.1.

## 1.4.1

* Ask `cargo metadata` to not access the network.  All the info we need is in `Cargo.toml`.
* Update dependencies.

## 1.4.0

* Support intralinks to methods and other `impl` items.
* Update dependencies.

## 1.3.0

* Support intralinks in reference-style links.
* Update dependencies.

## 1.2.0

* Allow the heading base level to be configurable.
* Update dependencies.

## 1.1.0

* Support backticked intralinks.
* Print a helpful message when the README is lacking a cargo-rdme marker.
* Update dependencies.

## 1.0.2

* Update dependencies.

## 1.0.1

* Do not depend on openssl (which is a transitive dependency of git2).  This improves the building speed, but most
  importantly we don't fail to build if the host system openssl is not supported by rust-openssl.

## 1.0.0

* Change the status code for the "check mode" (flag `--check`):
  * Exit code `3` means the README does not match the documentation.  In previous releases the exit code was `4`.
  * Exit code `4` means warnings were emitted.  In previous releases the exit code was `3`.
* Update dependencies.

## 0.7.3

* Update dependencies, in particular we are now using clap 4.

## 0.7.2

* Support these lib crate types: `proc-macro`, `dylib`, `staticlib`, `cdylib`, and `rlib`.
* Update dependencies.

## 0.7.1

* Update dependencies.

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
