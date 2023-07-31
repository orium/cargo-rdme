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
#![allow(clippy::new_without_default)]
#![allow(clippy::struct_excessive_bools)]

use crate::markdown::{Markdown, MarkdownError};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use thiserror::Error;

mod extract_doc;
mod inject_doc;
mod markdown;
pub mod transform;
pub mod utils;

pub use extract_doc::{extract_doc_from_source_file, ExtractDocError};
pub use inject_doc::{inject_doc_in_readme, InjectDocError, MARKER_RDME};

#[derive(Error, Debug)]
pub enum ProjectError {
    #[error("failed to get cargo metadata: {0}")]
    CargoMetadataError(cargo_metadata::Error),
    #[error("project has no root package")]
    ProjectHasNoRootPackage,
    #[error("project has no package \"{0}\"")]
    ProjectHasNoPackage(String),
}

impl From<cargo_metadata::Error> for ProjectError {
    fn from(e: cargo_metadata::Error) -> ProjectError {
        ProjectError::CargoMetadataError(e)
    }
}

pub fn find_first_file_in_ancestors(dir_path: impl AsRef<Path>, filename: &str) -> Option<PathBuf> {
    for ancestor_dir in dir_path.as_ref().ancestors() {
        let file = ancestor_dir.join(filename);
        if file.is_file() {
            return Some(file);
        }
    }

    None
}

#[derive(PartialEq, Eq, Debug)]
pub struct Project {
    package_name: String,
    readme_path: Option<PathBuf>,
    lib_path: Option<PathBuf>,
    bin_path: HashMap<String, PathBuf>,
    directory: PathBuf,
}

impl Project {
    /// Creates a [`Project`] the current directory.  It will search ancestor paths until it finds
    /// the root of the project.
    pub fn from_current_dir() -> Result<Project, ProjectError> {
        let metadata = Project::get_cargo_metadata()?;
        let package = metadata.root_package().ok_or(ProjectError::ProjectHasNoRootPackage)?;

        Ok(Project::from_package(package))
    }

    fn get_cargo_metadata() -> Result<cargo_metadata::Metadata, ProjectError> {
        let mut cmd = cargo_metadata::MetadataCommand::new();
        // We kindly ask cargo not to access the network.  All information we need is available
        // locally.
        let cmd = cmd.no_deps().other_options(["--offline".to_owned()]);
        Ok(cmd.exec()?)
    }

    fn select_package<'a>(
        metadata: &'a cargo_metadata::Metadata,
        package_name: &str,
    ) -> Option<&'a cargo_metadata::Package> {
        let package = metadata.packages.iter().find(|package| package.name == package_name)?;

        // We need to make sure the package we found is actually a project of the workspace.
        match metadata.workspace_members.contains(&package.id) {
            false => None,
            true => Some(package),
        }
    }

    pub fn from_current_dir_workspace_project(project_name: &str) -> Result<Project, ProjectError> {
        let metadata = Project::get_cargo_metadata()?;

        let package = Project::select_package(&metadata, project_name)
            .ok_or_else(|| ProjectError::ProjectHasNoPackage(project_name.to_owned()))?;

        Ok(Project::from_package(package))
    }

    fn from_package(package: &cargo_metadata::Package) -> Project {
        const LIB_CRATE_KINDS: [&str; 6] =
            ["lib", "dylib", "staticlib", "cdylib", "rlib", "proc-macro"];
        let lib_packages: Vec<&cargo_metadata::Target> = package
            .targets
            .iter()
            .filter(|target| target.kind.iter().any(|k| LIB_CRATE_KINDS.contains(&k.as_str())))
            .collect();

        assert!(lib_packages.len() <= 1, "more than one lib target");

        let lib_package = lib_packages.first();

        let bin_packages =
            package.targets.iter().filter(|target| target.kind.contains(&"bin".to_owned()));

        let directory = package
            .manifest_path
            .clone()
            .into_std_path_buf()
            .parent()
            .expect("error getting the parent path of the manifest file")
            .to_path_buf();

        Project {
            package_name: package.name.clone(),
            readme_path: package.readme.as_ref().map(|p| p.clone().into_std_path_buf()),
            lib_path: lib_package.map(|t| t.src_path.clone().into_std_path_buf()),
            bin_path: bin_packages
                .map(|t| (t.name.clone(), t.src_path.clone().into_std_path_buf()))
                .collect(),
            directory,
        }
    }

    #[must_use]
    pub fn get_lib_entryfile_path(&self) -> Option<&Path> {
        self.lib_path.as_ref().filter(|p| p.is_file()).map(PathBuf::as_path)
    }

    #[must_use]
    pub fn get_bin_default_entryfile_path(&self) -> Option<&Path> {
        match self.bin_path.len() {
            1 => self
                .bin_path
                .keys()
                .next()
                .and_then(|bin_name| self.get_bin_entryfile_path(bin_name)),
            _ => None,
        }
    }

    #[must_use]
    pub fn get_bin_entryfile_path(&self, name: &str) -> Option<&Path> {
        self.bin_path.get(name).filter(|p| p.is_file()).map(PathBuf::as_path)
    }

    #[must_use]
    pub fn get_readme_path(&self) -> Option<PathBuf> {
        self.readme_path
            .clone()
            .or_else(|| Some(Path::new("README.md").to_path_buf()))
            .map(|p| self.directory.join(p))
            .filter(|p| p.is_file())
    }

    #[must_use]
    pub fn get_package_name(&self) -> &str {
        &self.package_name
    }
}

fn project_package_name(manifest_path: impl AsRef<Path>) -> Option<String> {
    let str: String = std::fs::read_to_string(&manifest_path).ok()?;
    let toml: toml::Value = toml::from_str(&str).ok()?;
    let package_name =
        toml.get("package").and_then(|v| v.get("name")).and_then(toml::Value::as_str)?;

    Some(package_name.to_owned())
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct Doc {
    pub markdown: Markdown,
}

impl Doc {
    #[must_use]
    pub fn from_markdown(markdown: Markdown) -> Doc {
        Doc { markdown }
    }

    // TODO implement FromStr when ! type is stable.
    #[allow(clippy::should_implement_trait)]
    pub fn from_str(str: impl Into<String>) -> Doc {
        Doc { markdown: Markdown::from_str(str) }
    }

    fn is_toplevel_doc(attr: &syn::Attribute) -> bool {
        use syn::token::Not;
        use syn::AttrStyle;

        attr.style == AttrStyle::Inner(Not::default()) && attr.path().is_ident("doc")
    }

    pub fn lines(&self) -> impl Iterator<Item = &str> {
        self.markdown.lines()
    }

    // Return the markdown as a string.  Note that the line terminator will always be a line feed.
    #[must_use]
    pub fn as_string(&self) -> &str {
        self.markdown.as_string()
    }
}

#[derive(Error, Debug)]
pub enum ReadmeError {
    #[error("failed to read README file \"{0}\"")]
    ErrorReadingReadmeFromFile(PathBuf),
    #[error("failed to write README file \"{0}\"")]
    ErrorWritingMarkdownToFile(PathBuf),
    #[error("failed to write README")]
    ErrorWritingMarkdown,
}

impl From<MarkdownError> for ReadmeError {
    fn from(e: MarkdownError) -> ReadmeError {
        match e {
            MarkdownError::ErrorReadingMarkdownFromFile(p) => {
                ReadmeError::ErrorReadingReadmeFromFile(p)
            }
            MarkdownError::ErrorWritingMarkdownToFile(p) => {
                ReadmeError::ErrorWritingMarkdownToFile(p)
            }
            MarkdownError::ErrorWritingMarkdown => ReadmeError::ErrorWritingMarkdown,
        }
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum LineTerminator {
    Lf,
    CrLf,
}

pub struct Readme {
    pub markdown: Markdown,
}

impl Readme {
    pub fn from_file(file_path: impl AsRef<Path>) -> Result<Readme, ReadmeError> {
        Ok(Readme { markdown: Markdown::from_file(file_path)? })
    }

    // TODO implement FromStr when ! type is stable.
    #[allow(clippy::should_implement_trait)]
    pub fn from_str(str: impl Into<String>) -> Readme {
        Readme { markdown: Markdown::from_str(str) }
    }

    pub fn from_lines(lines: &[impl AsRef<str>]) -> Readme {
        Readme { markdown: Markdown::from_lines(lines) }
    }

    pub fn lines(&self) -> impl Iterator<Item = &str> {
        self.markdown.lines()
    }

    pub fn write_to_file(
        &self,
        file: impl AsRef<Path>,
        line_terminator: LineTerminator,
    ) -> Result<(), ReadmeError> {
        Ok(self.markdown.write_to_file(file, line_terminator)?)
    }

    pub fn write(
        &self,
        writer: impl std::io::Write,
        line_terminator: LineTerminator,
    ) -> Result<(), ReadmeError> {
        Ok(self.markdown.write(writer, line_terminator)?)
    }

    // Return the markdown as a string.  Note that the line terminator will always be a line feed.
    #[must_use]
    pub fn as_string(&self) -> &str {
        self.markdown.as_string()
    }
}

pub fn infer_line_terminator(file_path: impl AsRef<Path>) -> std::io::Result<LineTerminator> {
    let content: String = std::fs::read_to_string(file_path.as_ref())?;

    let crlf_lines: usize = content.matches("\r\n").count();
    let lf_lines: usize = content.matches('\n').count() - crlf_lines;

    if crlf_lines > lf_lines {
        Ok(LineTerminator::CrLf)
    } else {
        Ok(LineTerminator::Lf)
    }
}
