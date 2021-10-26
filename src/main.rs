/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#![cfg_attr(feature = "fatal-warnings", deny(warnings))]
#![deny(clippy::correctness)]
#![warn(clippy::pedantic)]
#![allow(clippy::match_bool)]
#![allow(clippy::if_not_else)]
#![allow(clippy::module_name_repetitions)]
#![allow(clippy::similar_names)]
#![allow(clippy::use_self)]
#![allow(clippy::single_match_else)]
#![allow(clippy::inline_always)]
#![allow(clippy::partialeq_ne_impl)]
#![allow(clippy::missing_errors_doc)]
#![allow(clippy::non_ascii_literal)]
#![allow(clippy::enum_variant_names)]
// Note: If you change this remember to update `README.md`.  To do so run `cargo run`.
//! # Cargo rdme
//!
//! Cargo command to create your README from your crate’s documentation.
//!
//! ## Usage
//!
//! Cargo rdme will insert your crate’s documentation in your README file.  To control where the
//! documentation will be inserted you need to insert a marker: `<!-- cargo-rdme -->`.  For example,
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
//! ## Config file
//!
//! If the default behavior of `cargo rdme` is not appropriate for your project you can crate a
//! configuration file `.cargo-rdme.toml` in the root of your project.  This is how that
//! configuration file can look like:
//!
//! ```toml
//! # What line terminator to use when updating the README file.  This can be "lf" or "crlf".
//! line-terminator = "lf"
//!
//! # The default entrypoint will be `src/lib.rs`.  You can change that in the `entrypoint` table.
//! [entrypoint]
//! # The entrypoint type can be "lib" or "bin".
//! type = "bin"
//! # When you set type to "bin" the entrypoint default to `src/main.rs`.  If you have binary targets
//! # specified in your cargo manifest you can select them by name with `bin-name`.
//! bin-name = "my-bin-name"
//! ```
//!
//! These setting can be overridden with command line flags.  Run `cargo rdme --help` for more
//! information.
//!
//! ## Integration with CI
//!
//! To verify that your README is up to date with your crate’s documentation you can run
//! `cargo rdme --check`.  The exit code will be `0` if the README is up to date, or `2` if it’s
//! not.

use crate::options::{EntrypointOpt, LineTerminatorOpt};
use cargo_rdme::{
    extract_doc_from_source_file, infer_line_terminator, inject_doc_in_readme, LineTerminator,
    Project,
};
use cargo_rdme::{Doc, ProjectError, Readme};
use std::path::{Path, PathBuf};
use thiserror::Error;

mod options;

const EXIT_CODE_ERROR: i32 = 1;
/// Exit code when we run in "check mode" and the README is not up to date.
const EXIT_CODE_CHECK: i32 = 2;

#[derive(Error, Debug)]
enum RunError {
    #[error("failed to get project info: {0}")]
    ProjectError(cargo_rdme::ProjectError),
    #[error("failed to extract rust doc: {0}")]
    ExtractDocError(cargo_rdme::ExtractDocError),
    #[error("failed to process README: {0}")]
    ReadmeError(cargo_rdme::ReadmeError),
    #[error("failed get crate's entry source file")]
    NoEntrySourceFile,
    #[error("failed get crate's README file")]
    NoReadmeFile,
    #[error("no crate-level rustdoc found")]
    NoRustdoc,
    #[error("failed to inject the documentation in the README: {0}")]
    InjectDocError(cargo_rdme::InjectDocError),
    #[error("IO error: {0}")]
    IOError(std::io::Error),
}

impl From<cargo_rdme::ProjectError> for RunError {
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

fn entrypoint(project: &Project, entrypoint_opt: EntrypointOpt) -> Option<PathBuf> {
    match entrypoint_opt {
        EntrypointOpt::Auto => {
            project.get_lib_entryfile_path().or_else(|| project.get_bin_default_entryfile_path())
        }
        EntrypointOpt::Lib => project.get_lib_entryfile_path(),
        EntrypointOpt::BinDefault => project.get_bin_default_entryfile_path(),
        EntrypointOpt::BinName(name) => project.get_bin_entryfile_path(&name),
    }
}

fn run(current_dir_path: impl AsRef<Path>, options: options::Options) -> Result<(), RunError> {
    let project: Project = Project::from_dir(current_dir_path)?;
    let entryfile: PathBuf =
        entrypoint(&project, options.entrypoint).ok_or(RunError::NoEntrySourceFile)?;
    let doc: Doc = match extract_doc_from_source_file(entryfile)? {
        None => return Err(RunError::NoRustdoc),
        Some(doc) => doc,
    };
    let readme_path: PathBuf = project.get_readme_path().ok_or(RunError::NoReadmeFile)?;
    let original_readme: Readme = Readme::from_file(&readme_path)?;
    let new_readme: Readme = inject_doc_in_readme(&original_readme, &doc)?;

    let line_terminator = match options.line_terminator {
        LineTerminatorOpt::Auto => infer_line_terminator(&readme_path)?,
        LineTerminatorOpt::Lf => LineTerminator::Lf,
        LineTerminatorOpt::CrLf => LineTerminator::CrLf,
    };

    match options.check {
        false => new_readme.write_to_file(&readme_path, line_terminator)?,
        true => {
            if !is_readme_up_to_date(&readme_path, &new_readme, line_terminator)? {
                eprintln!("README is not up to date.");
                std::process::exit(EXIT_CODE_CHECK);
            }
        }
    };

    Ok(())
}

fn main() {
    let cmd_options = options::cmd_options();

    match std::env::current_dir() {
        Ok(current_dir) => {
            let config_file_options = match options::config_file_options(&current_dir) {
                Ok(opts) => opts,
                Err(e) => {
                    eprintln!("error: unable to read config file: {}", e);
                    std::process::exit(EXIT_CODE_ERROR);
                }
            };

            let options = options::merge_options(cmd_options, config_file_options);

            if let Err(e) = run(current_dir, options) {
                eprintln!("error: {}", e);
                std::process::exit(EXIT_CODE_ERROR);
            }
        }
        Err(e) => {
            eprintln!("error: unable to get current directory: {}", e);
            std::process::exit(EXIT_CODE_ERROR);
        }
    }
}
