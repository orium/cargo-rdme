/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use itertools::Itertools;
use std::fs::File;
use std::path::{Path, PathBuf};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum MarkdownError {
    #[error("failed to read markdown file \"{0}\"")]
    ErrorReadingMarkdown(PathBuf),
    #[error("failed to write markdown file \"{0}\"")]
    ErrorWritingMarkdown(PathBuf),
}

pub struct Markdown {
    content: String,
}

impl Markdown {
    pub fn from_file(file: impl AsRef<Path>) -> Result<Markdown, MarkdownError> {
        let content: String = std::fs::read_to_string(file.as_ref())
            .map_err(|_| MarkdownError::ErrorReadingMarkdown(file.as_ref().to_path_buf()))?;

        Ok(Markdown::from_str(content))
    }

    pub fn from_str(str: impl Into<String>) -> Markdown {
        Markdown { content: str.into() }
    }

    pub fn from_lines(lines: &[impl AsRef<str>]) -> Markdown {
        let mut str = lines.iter().map(AsRef::as_ref).join("\n");

        // Lines must always end in newlines.
        if !str.ends_with('\n') {
            str.push('\n')
        }

        Markdown::from_str(str)
    }

    pub fn lines(&self) -> impl Iterator<Item = &str> {
        self.content.lines()
    }

    pub fn content_str(&self) -> &str {
        &self.content
    }

    pub fn write_to_file(&self, file: impl AsRef<Path>) -> Result<(), MarkdownError> {
        use std::io::prelude::*;

        File::create(&file)
            .and_then(|mut f| f.write_all(self.content.as_bytes()))
            .map_err(|_| MarkdownError::ErrorWritingMarkdown(file.as_ref().to_path_buf()))
    }
}
