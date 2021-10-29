/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

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

#[derive(Eq, PartialEq)]
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

pub mod parsing_utils {
    use itertools::Itertools;
    use std::ops::Range;

    #[derive(Eq, PartialEq, Debug, Clone)]
    pub struct Span {
        pub start: usize,
        pub end: usize,
    }

    impl From<Range<usize>> for Span {
        fn from(range: Range<usize>) -> Self {
            Span { start: range.start, end: range.end }
        }
    }

    #[derive(PartialEq, Eq, Debug)]
    pub enum ItemOrOther<'a, T> {
        Item(T),
        Other(&'a str),
    }

    pub struct MarkdownItemIterator<'a, T> {
        source: &'a str,
        iter: Box<dyn Iterator<Item = (Span, T)> + 'a>,
    }

    impl<'a, T> MarkdownItemIterator<'a, T> {
        pub fn new(
            source: &'a str,
            iter: impl Iterator<Item = (Span, T)> + 'a,
        ) -> MarkdownItemIterator<'a, T> {
            MarkdownItemIterator { source, iter: Box::new(iter) }
        }

        pub fn complete(self) -> impl Iterator<Item = ItemOrOther<'a, T>>
        where
            T: Clone,
        {
            use std::iter::once;

            once(None)
                .chain(self.iter.map(|e| Some(e)))
                .chain(once(None))
                .tuple_windows()
                .flat_map(|(l, r)| match (l, r) {
                    (None, Some((span, _))) => {
                        [ItemOrOther::Other(&self.source[0..span.start]), ItemOrOther::Other("")]
                    }
                    (Some((span_1, v_1)), Some((span_2, _))) => [
                        ItemOrOther::Item(v_1),
                        ItemOrOther::Other(&self.source[span_1.end..span_2.start]),
                    ],
                    (Some((span, v)), None) => {
                        [ItemOrOther::Item(v), ItemOrOther::Other(&self.source[span.end..])]
                    }
                    (None, None) => [ItemOrOther::Other(self.source), ItemOrOther::Other("")],
                })
                .filter(|e| match e {
                    ItemOrOther::Other("") => false,
                    _ => true,
                })
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use pretty_assertions::assert_eq;

        #[test]
        fn test_markdown_item_iterator_complete_no_item() {
            let str = "hello world";

            let mut iter = MarkdownItemIterator::<()>::new(str, std::iter::empty()).complete();

            assert_eq!(iter.next(), Some(ItemOrOther::Other("hello world")));
            assert_eq!(iter.next(), None);
        }

        #[test]
        fn test_markdown_item_iterator_complete_item_start() {
            let str = "_hello world";

            let items = [(Span::from(0..1), "x")];

            let mut iter = MarkdownItemIterator::new(str, items.into_iter()).complete();

            assert_eq!(iter.next(), Some(ItemOrOther::Item("x")));
            assert_eq!(iter.next(), Some(ItemOrOther::Other("hello world")));
            assert_eq!(iter.next(), None);
        }

        #[test]
        fn test_markdown_item_iterator_complete_item_end() {
            let str = "hello world_";

            let items = [(Span::from(11..12), "x")];

            let mut iter = MarkdownItemIterator::new(str, items.into_iter()).complete();

            assert_eq!(iter.next(), Some(ItemOrOther::Other("hello world")));
            assert_eq!(iter.next(), Some(ItemOrOther::Item("x")));
            assert_eq!(iter.next(), None);
        }

        #[test]
        fn test_markdown_item_iterator_complete_item_middle() {
            let str = "hello _ world";

            let items = [(Span::from(6..7), "x")];

            let mut iter = MarkdownItemIterator::new(str, items.into_iter()).complete();

            assert_eq!(iter.next(), Some(ItemOrOther::Other("hello ")));
            assert_eq!(iter.next(), Some(ItemOrOther::Item("x")));
            assert_eq!(iter.next(), Some(ItemOrOther::Other(" world")));
            assert_eq!(iter.next(), None);
        }

        #[test]
        fn test_markdown_item_iterator_complete_item_multiple() {
            let str = "hello _ world _ !";

            let items = [(Span::from(6..7), "x"), (Span::from(14..15), "y")];

            let mut iter = MarkdownItemIterator::new(str, items.into_iter()).complete();

            assert_eq!(iter.next(), Some(ItemOrOther::Other("hello ")));
            assert_eq!(iter.next(), Some(ItemOrOther::Item("x")));
            assert_eq!(iter.next(), Some(ItemOrOther::Other(" world ")));
            assert_eq!(iter.next(), Some(ItemOrOther::Item("y")));
            assert_eq!(iter.next(), Some(ItemOrOther::Other(" !")));
            assert_eq!(iter.next(), None);
        }

        #[test]
        fn test_markdown_item_iterator_complete_consecutive() {
            let str = "hello __ world!";

            let items = [(Span::from(6..7), "x"), (Span::from(7..8), "y")];

            let mut iter = MarkdownItemIterator::new(str, items.into_iter()).complete();

            assert_eq!(iter.next(), Some(ItemOrOther::Other("hello ")));
            assert_eq!(iter.next(), Some(ItemOrOther::Item("x")));
            assert_eq!(iter.next(), Some(ItemOrOther::Item("y")));
            assert_eq!(iter.next(), Some(ItemOrOther::Other(" world!")));
            assert_eq!(iter.next(), None);
        }
    }
}
