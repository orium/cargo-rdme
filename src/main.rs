// Note: If you change this remember to update `README.md`. To do so run `cargo run`.
//! Cargo command to create your README from your crate’s documentation.
//!
//! # Installation
//!
//! You can install cargo rdme with cargo by running `cargo install cargo-rdme`.
//!
//! Cargo rdme uses rustdoc to resolve [intralinks](#intralinks), which requires a specific
//! nightly rust toolchain (pinned by cargo rdme).
//! Install it with `cargo rdme install-rust-toolchain-for-intralinks`.
//!
//! # Usage
//!
//! Cargo rdme will insert your crate’s documentation in your README file. To control where the
//! documentation will be inserted you need to insert a marker: `<!-- cargo-rdme -->`. For example,
//! you can start your README with some glorious badges and follow up with the rustdoc
//! documentation:
//!
//! ```markdown
//! [![Build Status](https://example.org/badge.svg)](https://example.org/link-to-ci)
//!
//! <!-- cargo-rdme -->
//! ```
//!
//! After running `cargo rdme` you will find your README to be something like:
//!
//! ```markdown
//! [![Build Status](https://example.org/badge.svg)](https://example.org/link-to-ci)
//!
//! <!-- cargo-rdme start -->
//!
//! <WHATEVER-YOUR-CRATES-DOC-IS>
//!
//! <!-- cargo-rdme end -->
//! ```
//!
//! Whenever change your crate’s documentation you just need to run `cargo rdme` to update your
//! README file.
//!
//! # Automatic transformations
//!
//! The documentation of your crate doesn’t always map directly to a good README. For example,
//! rust code blocks can have hidden lines. Those should not be shown in the README file.
//!
//! This section covers the transformation cargo rdme automatically apply to generate a better
//! README.
//!
//! ## Rust code block
//!
//! Rust code block are transformed in two ways by cargo rdme:
//!
//! 1. Rust code blocks with lines starting with `#` will be omitted, just like in `rustdoc`.
//! 2. Rust code blocks get annotated with the `rust` markdown tag so it gets proper syntax
//!    highlighting. We also remove tags that only concern `rustdoc` such as `should_panic`.
//!
//! In the table below you can see an example of these modification. The code block now is
//! tagged with `rust` and hidden lines were removed:
//!
//! <table border="1">
//! <col span="1" width="40%">
//! <col span="1" width="40%">
//! </colgroup>
//! <tr>
//! <th><center>Crate’s rustdoc</center></th>
//! <th><center>README.md</center></th>
//! <tr>
//! <tr>
//! <td>
//!
//! ```rust
//! //! To check if a number is prime do:
//! //!
//! //! ```
//! //! # fn main() {
//! //! for i in 2.. {
//! //!     if is_prime(i) {
//! //!         println!("{i}");
//! //!     }
//! //! }
//! //! # }
//! //! ```
//! ```
//!
//! </td>
//! <td>
//!
//! ````markdown
//! To check if a number is prime do:
//!
//! ```rust
//! for i in 2.. {
//!     if is_prime(i) {
//!         println!("{i}");
//!     }
//! }
//! ```
//! ````
//!
//! </td>
//! </tr>
//! </table>
//!
//! ## Intralinks
//!
//! Rust documentation can contain [links to items defined in the crate](https://doc.rust-lang.org/stable/rustdoc/linking-to-items-by-name.html).
//! This links would not make sense in your README file, so cargo rdme automatically generate
//! links to [docs.rs](https://docs.rs) for these intralinks.
//!
//! Take a look at the example below:
//!
//! <table border="1">
//! <col span="1" width="40%">
//! <col span="1" width="40%">
//! </colgroup>
//! <tr>
//! <th><center>Crate’s rustdoc</center></th>
//! <th><center>README.md</center></th>
//! <tr>
//! <tr>
//! <td>
//!
//! ```rust
//! //! To check if a number is prime use
//! //! [`is_prime`](crate::is_prime).
//! ```
//!
//! </td>
//! <td>
//!
//! ```markdown
//! To check if a number is prime use
//! [`is_prime`](https://docs.rs/prime/latest/prime/fn.is_prime.html).
//! ```
//!
//! </td>
//! </tr>
//! </table>
//!
//! To resolve intralinks, cargo rdme invokes `rustdoc` on your crate and consumes its JSON output.
//! It requires a specific nightly rust toolchain, because rustdoc’s JSON output is unstable (see
//! [rust-lang/rust#76578](https://github.com/rust-lang/rust/issues/76578)) and can break between
//! nightly updates. Install it with `cargo rdme install-rust-toolchain-for-intralinks`.
//!
//! ## Heading levels
//!
//! The heading levels in the crate’s documentation will, by default, be nested under the level
//! of the section of the README where it is inserted into. This behavior can be changed with
//! the `--heading-base-level` command line flag, or in the configuration file (see example
//! below).
//!
//! # Configuration file
//!
//! If the default behavior of `cargo rdme` is not appropriate for your project you can crate a
//! configuration file `.cargo-rdme.toml` in the root of your project. This is how that
//! configuration file can look like:
//!
//! ```toml
//! # Override the README file path. When this is not set cargo rdme will use the file path defined
//! # in the project’s `Cargo.toml`.
//! readme-path = "MY-README.md"
//!
//! # What line terminator to use when generating the README file. This can be "lf" or "crlf".
//! line-terminator = "lf"
//!
//! # If you are using a workspace to hold multiple projects, use this to select the project from
//! # which to extract the documentation from. It can be useful to also set `readme-path` to create
//! # the README file in the root of the project.
//! workspace-project = "subproject"
//!
//! # Defines the base heading level to use when inserting the crate’s documentation in the
//! # README. If this is not set the crate’s documentation will be inserted with its sections
//! # belonging to the README section where the insertion happens.
//! heading-base-level = 0
//!
//! # The default entrypoint will be `src/lib.rs`. You can change that in the `entrypoint` table.
//! [entrypoint]
//! # The entrypoint type can be "lib" or "bin".
//! type = "bin"
//! # When you set type to "bin" the entrypoint default to `src/main.rs`. If you have binary targets
//! # specified in your cargo manifest you can select them by name with `bin-name`.
//! bin-name = "my-bin-name"
//!
//! [intralinks]
//! # Defines the base url to use in intralinks urls. The default value is `https://docs.rs`.
//! docs-rs-base-url = "https://mydocs.rs"
//! # Defines the version to use in intralinks urls. The default value is `latest`.
//! docs-rs-version = "1.0.0"
//! # If this is set the intralinks will be stripping in the README file.
//! strip-links = false
//!
//! # Enable all features when calling rustdoc to resolve intralinks.
//! # all-features = true
//! # Features to enable when calling rustdoc to resolve intralinks.
//! features = ["foo", "bar"]
//! # Disable default features when calling rustdoc to resolve intralinks.
//! no-default-features = false
//! ```
//!
//! These setting can be overridden with command line flags. Run `cargo rdme --help` for more
//! information.
//!
//! # Integration with CI
//!
//! To verify that your README is up to date with your crate’s documentation you can run
//! `cargo rdme --check`. The exit code will be `0` if the README is up to date, `3` if it’s
//! not, or `4` if there were warnings.
//!
//! If you use GitHub Actions you can add this step to verify if the README is up to date:
//!
//! ```yaml
//! - name: Check if the README is up to date.
//!   run: |
//!     cargo install cargo-rdme
//!     # If you use intralinks, install the specific nightly rust toolchain cargo-rdme requires:
//!     cargo rdme install-rust-toolchain-for-intralinks
//!     cargo rdme --check
//! ```

use crate::options::{EntrypointOpt, LineTerminatorOpt};
use cargo_rdme::transform::IntralinkError;
use cargo_rdme::{Doc, ProjectError, Readme};
use cargo_rdme::{
    LineTerminator, PackageTarget, Project, extract_doc_from_source_file, infer_line_terminator,
    inject_doc_in_readme,
};
use std::cell::Cell;
use std::path::{Path, PathBuf};
use thiserror::Error;

#[macro_use]
mod console;
mod options;

enum ExitCode {
    Ok = 0,
    Error = 1,
    /// Exit code we don't update the README because we would overwrite uncommitted changes.
    ReadmeNotUpdatedUncommittedChanges = 2,
    /// Exit code when we run in "check mode" and the README is not up to date.
    CheckMismatch = 3,
    /// Exit code when we run in "check mode" and there were warnings.
    CheckHasWarnings = 4,
}

impl From<RunError> for ExitCode {
    fn from(value: RunError) -> ExitCode {
        match value {
            RunError::ProjectError(_)
            | RunError::ExtractDocError(_)
            | RunError::ReadmeError(_)
            | RunError::NoEntrySourceFile
            | RunError::NoTargetPackage
            | RunError::NoReadmeFile
            | RunError::NoRustdoc
            | RunError::InjectDocError(_)
            | RunError::TransformIntraLinkError(_)
            | RunError::IOError(_) => ExitCode::Error,
            RunError::ReadmeNotUpdatedUncommittedChanges => {
                ExitCode::ReadmeNotUpdatedUncommittedChanges
            }
            RunError::CheckReadmeMismatch => ExitCode::CheckMismatch,
            RunError::CheckHasWarnings => ExitCode::CheckHasWarnings,
        }
    }
}

#[derive(Error, Debug)]
enum RunError {
    #[error("failed to get project info: {0}")]
    ProjectError(ProjectError),
    #[error("failed to extract rust doc: {0}")]
    ExtractDocError(cargo_rdme::ExtractDocError),
    #[error("failed to process README: {0}")]
    ReadmeError(cargo_rdme::ReadmeError),
    #[error("failed to get crate's entry source file")]
    NoEntrySourceFile,
    #[error("failed to get crate's target package")]
    NoTargetPackage,
    #[error("crate's README file not found")]
    NoReadmeFile,
    #[error("crate-level rustdoc not found")]
    NoRustdoc,
    #[error("failed to inject the documentation in the README: {0}")]
    InjectDocError(cargo_rdme::InjectDocError),
    #[error("IO error: {0}")]
    IOError(std::io::Error),
    #[error("not updating README: it has uncommitted changes (use `--force` to bypass this check)")]
    ReadmeNotUpdatedUncommittedChanges,
    #[error("failed to transform intralinks: {0}")]
    TransformIntraLinkError(IntralinkError),
    #[error("README is not up to date")]
    CheckReadmeMismatch,
    #[error("README is up to date, but warnings were emitted")]
    CheckHasWarnings,
}

impl From<ProjectError> for RunError {
    fn from(e: ProjectError) -> RunError {
        RunError::ProjectError(e)
    }
}

impl From<cargo_rdme::ExtractDocError> for RunError {
    fn from(e: cargo_rdme::ExtractDocError) -> RunError {
        RunError::ExtractDocError(e)
    }
}

impl From<cargo_rdme::ReadmeError> for RunError {
    fn from(e: cargo_rdme::ReadmeError) -> RunError {
        RunError::ReadmeError(e)
    }
}

impl From<cargo_rdme::InjectDocError> for RunError {
    fn from(e: cargo_rdme::InjectDocError) -> RunError {
        RunError::InjectDocError(e)
    }
}

impl From<std::io::Error> for RunError {
    fn from(e: std::io::Error) -> RunError {
        RunError::IOError(e)
    }
}

impl From<IntralinkError> for RunError {
    fn from(e: IntralinkError) -> RunError {
        RunError::TransformIntraLinkError(e)
    }
}

impl From<std::convert::Infallible> for RunError {
    fn from(_: std::convert::Infallible) -> RunError {
        unreachable!()
    }
}

/// Check if the README is up to date.
///
/// This will check if the README has the given line terminator as well.
fn is_readme_up_to_date(
    readme_path: impl AsRef<Path>,
    new_readme: &Readme,
    line_terminator: LineTerminator,
) -> Result<bool, RunError> {
    let current_readme_raw: String = std::fs::read_to_string(readme_path)?;
    let new_readme_raw: Vec<u8> = {
        let mut bytes: Vec<u8> = Vec::with_capacity(32 * 1024);
        new_readme.write(&mut bytes, line_terminator)?;
        bytes
    };

    Ok(current_readme_raw.as_bytes() == new_readme_raw.as_slice())
}

fn entrypoint<'a>(project: &'a Project, entrypoint_opt: &EntrypointOpt) -> Option<&'a Path> {
    match entrypoint_opt {
        EntrypointOpt::Auto => {
            project.get_lib_entryfile_path().or_else(|| project.get_bin_default_entryfile_path())
        }
        EntrypointOpt::Lib => project.get_lib_entryfile_path(),
        EntrypointOpt::BinDefault => project.get_bin_default_entryfile_path(),
        EntrypointOpt::BinName(name) => project.get_bin_entryfile_path(name),
    }
}

fn package_target(project: &Project, entrypoint_opt: &EntrypointOpt) -> Option<PackageTarget> {
    let bin_default = || {
        project
            .get_bin_default_crate_name()
            .map(|name| PackageTarget::Bin { crate_name: name.to_owned() })
    };

    match entrypoint_opt {
        EntrypointOpt::Auto => {
            project.get_lib_entryfile_path().map(|_| PackageTarget::Lib).or_else(bin_default)
        }
        EntrypointOpt::Lib => Some(PackageTarget::Lib),
        EntrypointOpt::BinDefault => bin_default(),
        EntrypointOpt::BinName(name) => Some(PackageTarget::Bin { crate_name: name.clone() }),
    }
}

fn line_terminator(
    line_terminator_opt: LineTerminatorOpt,
    readme_path: impl AsRef<Path>,
) -> std::io::Result<LineTerminator> {
    match line_terminator_opt {
        LineTerminatorOpt::Auto => infer_line_terminator(readme_path),
        LineTerminatorOpt::Lf => Ok(LineTerminator::Lf),
        LineTerminatorOpt::CrLf => Ok(LineTerminator::CrLf),
    }
}

struct Warnings {
    had_warnings: bool,
}

fn transform_doc(
    doc: &Doc,
    project: &Project,
    package_target: PackageTarget,
    options: &options::Options,
) -> Result<(Doc, Warnings), RunError> {
    use cargo_rdme::transform::{
        DocTransform, DocTransformIntralinks, DocTransformRustMarkdownTag,
        DocTransformRustRemoveComments,
    };

    let transform = DocTransformRustRemoveComments::new();
    // TODO Use `into_ok()` once it is stable (https://github.com/rust-lang/rust/issues/61695).
    let doc = transform.transform(doc)?;

    let transform = DocTransformRustMarkdownTag::new();
    // TODO Use `into_ok()` once it is stable (https://github.com/rust-lang/rust/issues/61695).
    let doc = transform.transform(&doc)?;

    let had_warnings = Cell::new(false);
    let transform = DocTransformIntralinks::new(
        project.get_package_name().as_str().to_owned(),
        package_target,
        options.workspace_project.clone(),
        project.get_manifest_path().clone(),
        |msg| {
            print_warning!("{}", msg);
            had_warnings.set(true);
        },
        options.intralinks.clone(),
    );

    Ok((transform.transform(&doc)?, Warnings { had_warnings: had_warnings.into_inner() }))
}

/// Check if the `path` has local changes that were not yet commited.
///
/// This returns `None` if we were not able to determine that.
fn git_is_current(path: impl AsRef<Path>) -> Option<bool> {
    use gix::bstr::BString;

    let repo = gix::discover(path.as_ref().parent()?).ok()?;
    let work_dir = repo.workdir()?;
    let path_in_repo = path.as_ref().strip_prefix(work_dir).ok()?;

    // A file absent from the index is either untracked or gitignored. Either way, treat it as "not
    // current", to avoid silently overwriting a file that git is not tracking.
    let path_bstr = gix::bstr::BStr::new(path_in_repo.as_os_str().as_encoded_bytes());
    let index = repo.index().ok()?;
    if index.entry_by_path(path_bstr).is_none() {
        return Some(false);
    }

    // File is tracked. Check for staged or unstaged changes.
    let cwd = std::env::current_dir().ok()?;
    let path = path.as_ref().strip_prefix(&cwd).ok()?;
    let pattern = BString::from(path.as_os_str().as_encoded_bytes());

    let items: Result<Vec<_>, _> =
        repo.status(gix::progress::Discard).ok()?.into_iter([pattern]).ok()?.collect();

    Some(items.ok()?.is_empty())
}

fn update_readme(
    new_readme: &Readme,
    readme_path: impl AsRef<Path>,
    line_terminator: LineTerminator,
    ignore_uncommitted_changes: bool,
) -> Result<(), RunError> {
    match ignore_uncommitted_changes || git_is_current(&readme_path).unwrap_or(true) {
        true => Ok(new_readme.write_to_file(&readme_path, line_terminator)?),
        false => Err(RunError::ReadmeNotUpdatedUncommittedChanges),
    }
}

fn run(options: options::Options) -> Result<(), RunError> {
    let project: Project = match options.workspace_project {
        None => Project::from_current_dir(options.manifest_path.as_deref())?,
        Some(ref project) => {
            Project::from_current_dir_workspace_project(options.manifest_path.as_deref(), project)?
        }
    };
    let entryfile: &Path =
        entrypoint(&project, &options.entrypoint).ok_or(RunError::NoEntrySourceFile)?;
    let package_target: PackageTarget =
        package_target(&project, &options.entrypoint).ok_or(RunError::NoTargetPackage)?;
    let doc: Doc = match extract_doc_from_source_file(entryfile)? {
        None => return Err(RunError::NoRustdoc),
        Some(doc) => doc,
    };

    let (doc, warnings) = transform_doc(&doc, &project, package_target, &options)?;

    let readme_path: PathBuf = match options.readme_path {
        None => project.get_readme_path().ok_or(RunError::NoReadmeFile)?,
        Some(path) => {
            if !path.is_file() {
                return Err(RunError::NoReadmeFile);
            }
            path
        }
    };
    let original_readme: Readme = Readme::from_file(&readme_path)?;
    let new_readme = inject_doc_in_readme(&original_readme, &doc, options.heading_base_level)?;

    if !new_readme.had_marker {
        let msg = indoc::formatdoc! { "
            No marker found in the README file ({readme_filepath}).

            cargo-rdme expects a marker in the README where the crate’s documentation will
            be inserted. This is the marker you should add to your README:

            {marker}",
            readme_filepath = readme_path.display(),
            marker = cargo_rdme::MARKER_RDME,
        };
        print_info!("{}", msg);
    }

    let line_terminator = line_terminator(options.line_terminator, &readme_path)?;

    match options.check {
        false => update_readme(&new_readme.readme, readme_path, line_terminator, options.force),
        true => {
            if !is_readme_up_to_date(&readme_path, &new_readme.readme, line_terminator)? {
                return Err(RunError::CheckReadmeMismatch);
            }

            if warnings.had_warnings && !options.no_fail_on_warnings {
                return Err(RunError::CheckHasWarnings);
            }

            Ok(())
        }
    }
}

fn install_rust_toolchain_for_intralinks() -> Result<(), IntralinkError> {
    use cargo_rdme::transform::{
        EXPECTED_RUST_TOOLCHAIN, install_expected_rust_toolchain,
        is_expected_rust_toolchain_installed,
    };

    if is_expected_rust_toolchain_installed()? {
        print_info!("rust toolchain `{}` is already installed.", EXPECTED_RUST_TOOLCHAIN);
        return Ok(());
    }

    print_info!("installing rust toolchain `{}`...", EXPECTED_RUST_TOOLCHAIN);
    install_expected_rust_toolchain()?;
    print_info!("rust toolchain `{}` installed.", EXPECTED_RUST_TOOLCHAIN);

    Ok(())
}

fn main() {
    let exit_code: ExitCode = match options::command() {
        options::Command::InstallRustToolchainForIntralinks => {
            match install_rust_toolchain_for_intralinks() {
                Ok(()) => ExitCode::Ok,
                Err(e) => {
                    print_error!("failed to install rust toolchain: {}", e);
                    ExitCode::Error
                }
            }
        }
        options::Command::Run(cmd_options) => {
            let directory = cmd_options
                .manifest_path()
                .and_then(|path| path.parent())
                .map_or_else(std::env::current_dir, |p| Ok(p.to_owned()));

            match directory {
                Ok(dir) => match options::config_file_options(dir) {
                    Ok(config_file_options) => {
                        let options = options::merge_options(cmd_options, config_file_options);

                        match run(options) {
                            Ok(()) => ExitCode::Ok,
                            Err(e) => {
                                print_error!("{}", &e);
                                e.into()
                            }
                        }
                    }
                    Err(e) => {
                        print_error!("unable to read config file: {}", e);
                        ExitCode::Error
                    }
                },
                Err(e) => {
                    print_error!("unable to get current directory: {}", e);
                    ExitCode::Error
                }
            }
        }
    };

    std::process::exit(exit_code as i32);
}
