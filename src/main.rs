/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#![cfg_attr(feature = "fatal-warnings", deny(warnings))]
#![deny(clippy::correctness)]
#![warn(clippy::pedantic)]
#![allow(clippy::match_bool)]
#![allow(clippy::if_not_else)]
#![allow(clippy::stutter)]
#![allow(clippy::similar_names)]
#![allow(clippy::use_self)]
#![allow(clippy::single_match_else)]
#![allow(clippy::inline_always)]
#![allow(clippy::partialeq_ne_impl)]
// Note: If you change this remember to update `README.md`.  To do so run `cargo rdme`.
//! # Cargo rdme
//!
//! Cargo command to create your `README.md` from your crate's documentation.

use crate::options::LineTerminatorOpt;
use cargo_rdme::{infer_line_terminator, inject_doc, LineTerminator, Project};
use cargo_rdme::{Doc, ProjectError, Readme};
use options::Options;
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
    #[error("failed to process rust doc: {0}")]
    DocError(cargo_rdme::DocError),
    #[error("failed to process README: {0}")]
    ReadmeError(cargo_rdme::ReadmeError),
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

impl From<cargo_rdme::DocError> for RunError {
    fn from(e: cargo_rdme::DocError) -> RunError {
        RunError::DocError(e)
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

fn run(current_dir: impl AsRef<Path>, options: Options) -> Result<(), RunError> {
    let project: Project = Project::from_path(current_dir)?;
    let entryfile: PathBuf = project.get_src_entryfile();
    let doc: Doc = match Doc::from_source_file(entryfile)? {
        None => return Err(RunError::NoRustdoc),
        Some(doc) => doc,
    };
    let readme_path: PathBuf = project.get_readme_path();
    let original_readme: Readme = Readme::from_file(&readme_path)?;
    let new_readme: Readme = inject_doc(&original_readme, &doc)?;

    let line_terminator = match options.line_terminator {
        Some(LineTerminatorOpt::Auto) | None => infer_line_terminator(project.get_readme_path())?,
        Some(LineTerminatorOpt::Lf) => LineTerminator::Lf,
        Some(LineTerminatorOpt::CrLf) => LineTerminator::CrLf,
    };

    match options.check {
        false => new_readme.write_to_file(project.get_readme_path(), line_terminator)?,
        true => {
            if !is_readme_up_to_date(readme_path, &new_readme, line_terminator)? {
                eprintln!("README is not up to date.");
                std::process::exit(EXIT_CODE_CHECK);
            }
        }
    };

    Ok(())
}

fn main() {
    let options = options::options_from_cmd();

    match std::env::current_dir() {
        Ok(current_dir) => {
            if let Err(e) = run(current_dir, options) {
                eprintln!("error: {}", e);
                std::process::exit(EXIT_CODE_ERROR);
            }
        }
        Err(_) => {
            eprintln!("error: unable to get current directory.");
            std::process::exit(EXIT_CODE_ERROR);
        }
    }
}
