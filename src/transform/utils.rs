/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use crate::markdown::Markdown;
use itertools::Itertools;
use std::ops::Range;

pub fn is_rust_code_block(tags: &str) -> bool {
    tags.split(',').all(|tag| match tag {
        "should_panic" | "no_run" | "ignore" | "allow_fail" | "rust" | "test_harness"
        | "compile_fail" | "" => true,
        tag if tag.starts_with("ignore-") => true,
        tag if tag.starts_with("edition") => true,
        _ => false,
    })
}

pub fn rust_code_block_iterator(markdown: &Markdown) -> MarkdownItemIterator<&str> {
    use pulldown_cmark::{CodeBlockKind, Event, Options, Parser, Tag};

    let source = markdown.as_string();
    let parser = Parser::new_ext(source, Options::all());

    let iter = parser.into_offset_iter().filter_map(move |(event, range)| match event {
        Event::Start(Tag::CodeBlock(CodeBlockKind::Indented)) => {
            Some((range.clone().into(), &source[range]))
        }
        Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(tags))) if is_rust_code_block(&tags) => {
            Some((range.clone().into(), &source[range]))
        }
        _ => None,
    });

    MarkdownItemIterator::new(source, iter)
}

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

    #[allow(dead_code)]
    pub fn items(self) -> impl Iterator<Item = T> + 'a
    where
        T: 'a,
    {
        self.iter.map(|(_, item)| item)
    }

    #[allow(dead_code)]
    pub fn items_with_spans(self) -> impl Iterator<Item = (Span, T)> + 'a
    where
        T: 'a,
    {
        self.iter
    }

    pub fn complete(self) -> impl Iterator<Item = ItemOrOther<'a, T>>
    where
        T: Clone,
    {
        use std::iter::once;

        once(None)
            .chain(self.iter.map(Some))
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
            .filter(|e| !matches!(e, ItemOrOther::Other("")))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_rust_code_block_iterator_items() {
        let doc = indoc! { r#"
            # The crate

            Look a this code:

            ```
            println!("first");
            ```

            ```rust
            println!("second");
            ```

            ```text
            Just some text.
            ```

            ```ignore,no_run
            println!("third");
            ```

            ```should_panic
            println!("fourth");
            ```

            That's ```all```!  Have a nice `day`!
            "#
        };
        let doc = Markdown::from_str(doc);

        let mut iter = rust_code_block_iterator(&doc).items();

        assert_eq!(iter.next(), Some("```\nprintln!(\"first\");\n```"));
        assert_eq!(iter.next(), Some("```rust\nprintln!(\"second\");\n```"));
        assert_eq!(iter.next(), Some("```ignore,no_run\nprintln!(\"third\");\n```"));
        assert_eq!(iter.next(), Some("```should_panic\nprintln!(\"fourth\");\n```"));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_rust_code_block_iterator_items_known_code_block_tags() {
        let tags = [
            "should_panic",
            "no_run",
            "ignore",
            "allow_fail",
            "rust",
            "test_harness",
            "compile_fail",
            "edition2018",
            "ignore-foo",
        ];

        for tag in tags {
            let doc = format!("Foo:\n```{}\nprintln!(\"There\");\n```\nEnd\n", tag);
            let expected_str = format!("```{}\nprintln!(\"There\");\n```", tag);

            let doc = Markdown::from_str(doc);

            let mut iter = rust_code_block_iterator(&doc).items();

            assert_eq!(iter.next(), Some(expected_str.as_str()));
            assert_eq!(iter.next(), None);
        }
    }

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
