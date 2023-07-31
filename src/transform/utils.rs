/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use crate::markdown::Markdown;
use crate::utils::MarkdownItemIterator;

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
            let doc = format!("Foo:\n```{tag}\nprintln!(\"There\");\n```\nEnd\n");
            let expected_str = format!("```{tag}\nprintln!(\"There\");\n```");

            let doc = Markdown::from_str(doc);

            let mut iter = rust_code_block_iterator(&doc).items();

            assert_eq!(iter.next(), Some(expected_str.as_str()));
            assert_eq!(iter.next(), None);
        }
    }
}
