use crate::LineTerminator;
use itertools::Itertools;
use std::fmt::Formatter;
use std::fs::File;
use std::path::{Path, PathBuf};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum MarkdownError {
    #[error("failed to read markdown file \"{0}\"")]
    ErrorReadingMarkdownFromFile(PathBuf),
    #[error("failed to write markdown file \"{0}\"")]
    ErrorWritingMarkdownToFile(PathBuf),
    #[error("failed to write markdown")]
    ErrorWritingMarkdown,
}

#[derive(Eq, PartialEq, Clone)]
pub struct Markdown {
    /// Content of the markdown.  The line terminator is always `\n`.
    content: String,
}

impl Markdown {
    pub fn from_file(file_path: impl AsRef<Path>) -> Result<Markdown, MarkdownError> {
        let content: String = std::fs::read_to_string(file_path.as_ref()).map_err(|_| {
            MarkdownError::ErrorReadingMarkdownFromFile(file_path.as_ref().to_path_buf())
        })?;

        Ok(Markdown::from_str(content))
    }

    pub fn from_str(str: impl Into<String>) -> Markdown {
        let mut content = str.into().replace("\r\n", "\n");

        // Lines must always end in newlines.
        if !content.ends_with('\n') {
            content.push('\n');
        }

        Markdown { content }
    }

    pub fn from_lines(lines: &[impl AsRef<str>]) -> Markdown {
        let str = lines.iter().map(AsRef::as_ref).join("\n");

        Markdown::from_str(str)
    }

    pub fn lines(&self) -> impl Iterator<Item = &str> {
        self.content.lines()
    }

    pub fn write_to_file(
        &self,
        file: impl AsRef<Path>,
        line_terminator: LineTerminator,
    ) -> Result<(), MarkdownError> {
        File::create(&file)
            .map_err(|_| MarkdownError::ErrorWritingMarkdownToFile(file.as_ref().to_path_buf()))
            .and_then(|f| self.write(f, line_terminator))
    }

    pub fn write(
        &self,
        mut writer: impl std::io::Write,
        line_terminator: LineTerminator,
    ) -> Result<(), MarkdownError> {
        let mut write_line = |line: &str| -> std::io::Result<()> {
            writer.write_all(line.as_bytes())?;
            match line_terminator {
                LineTerminator::Lf => writer.write_all("\n".as_bytes()),
                LineTerminator::CrLf => writer.write_all("\r\n".as_bytes()),
            }
        };

        for line in self.lines() {
            write_line(line).map_err(|_| MarkdownError::ErrorWritingMarkdown)?;
        }

        Ok(())
    }

    // Return the markdown as a string.  Note that the line terminator will always be a line feed.
    pub fn as_string(&self) -> &str {
        &self.content
    }
}

impl std::fmt::Debug for Markdown {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use std::fmt::Write;

        for line in self.lines() {
            f.write_str(line)?;
            f.write_char('\n')?;
        }

        Ok(())
    }
}
