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

use cargo_rdme::{inject_doc, line_terminator, Project};
use cargo_rdme::{Doc, ProjectError, Readme};
use std::path::{Path, PathBuf};
use thiserror::Error;

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

fn run(current_dir: impl AsRef<Path>) -> Result<(), RunError> {
    let project: Project = Project::from_path(current_dir)?;
    let entryfile: PathBuf = project.get_src_entryfile();
    let doc: Doc = match Doc::from_source_file(entryfile)? {
        None => return Err(RunError::NoRustdoc),
        Some(doc) => doc,
    };
    let original_readme: Readme = Readme::from_file(project.get_readme())?;
    let original_readme_line_terminator = line_terminator(project.get_readme())?;
    let new_readme = inject_doc(&original_readme, &doc)?;

    new_readme.write_to_file(project.get_readme(), original_readme_line_terminator)?;

    Ok(())
}

fn main() {
    match std::env::current_dir() {
        Ok(current_dir) => {
            if let Err(e) = run(current_dir) {
                eprintln!("error: {}", e);
                std::process::exit(1);
            }
        }
        Err(_) => {
            eprintln!("error: unable to get current directory.");
            std::process::exit(1);
        }
    }
}
