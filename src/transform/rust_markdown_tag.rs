/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use crate::transform::utils::rust_code_block_iterator;
use crate::transform::DocTransform;
use crate::utils::ItemOrOther;
use crate::Doc;
use std::convert::Infallible;

pub struct DocTransformRustMarkdownTag;

impl DocTransformRustMarkdownTag {
    #[must_use]
    pub fn new() -> DocTransformRustMarkdownTag {
        DocTransformRustMarkdownTag
    }
}

fn process_code_block(new_doc_str: &mut String, code_block: &str) {
    let fenced = code_block.starts_with("```");
    let mut base_indent = 0;

    if !fenced {
        while !new_doc_str.ends_with('\n') && !new_doc_str.is_empty() {
            new_doc_str.pop();
            base_indent += 1;
        }

        new_doc_str.push_str("```rust\n");
    }

    for (i, line) in code_block.split('\n').enumerate() {
        match i {
            0 if fenced => {
                assert!(
                    line.starts_with("```"),
                    "fenced code block does not start with triple backtick"
                );

                // A fence can have more than three backticks.  We need to preserve that, since
                // it can be used to escape triple fences inside the code block itself.
                // See https://stackoverflow.com/a/31834381.
                line.chars().take_while(|c| *c == '`').for_each(|c| new_doc_str.push(c));

                new_doc_str.push_str("rust");
            }
            0 => {
                new_doc_str.push_str(line);
            }
            _ => {
                new_doc_str.push('\n');

                if line.len() > base_indent {
                    new_doc_str.push_str(&line[base_indent..]);
                }
            }
        }
    }

    if !fenced {
        new_doc_str.push_str("```\n");
    }
}

impl DocTransform for DocTransformRustMarkdownTag {
    type E = Infallible;

    fn transform(&self, doc: &Doc) -> Result<Doc, Infallible> {
        let mut new_doc_str = String::new();

        for item_or_other in rust_code_block_iterator(&doc.markdown).complete() {
            match item_or_other {
                ItemOrOther::Item(code_block) => {
                    process_code_block(&mut new_doc_str, code_block);
                }
                ItemOrOther::Other(other) => {
                    new_doc_str.push_str(other);
                }
            }
        }

        Ok(Doc::from_str(new_doc_str))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_markdown_tag_no_code_block() {
        let doc_str = indoc! { r#"
            # The crate

            Look a this code:

            That's all!  Have a nice day!
            "#
        };

        let expected_str = indoc! { r#"
            # The crate

            Look a this code:

            That's all!  Have a nice day!
            "#
        };

        let doc = Doc::from_str(doc_str);
        let expected = Doc::from_str(expected_str);

        let transform = DocTransformRustMarkdownTag::new();

        assert_eq!(transform.transform(&doc).unwrap(), expected);
    }

    #[test]
    fn test_markdown_tag_fenced_code_block() {
        let doc_str = indoc! { r#"
            # The crate

            Look a this code:

            ```
            if true {
                println!("Hi");
            }
            println!("There");
            ```

            A second one:

            ```rust
            println!("Hi");
            ```

            And so one:

            ```text
            A bit of text here.
            ```

            And so forth:

            ```should_panic
            println!("Hi");
            ```

            That's all!  Have a nice day!
            "#
        };

        let expected_str = indoc! { r#"
            # The crate

            Look a this code:

            ```rust
            if true {
                println!("Hi");
            }
            println!("There");
            ```

            A second one:

            ```rust
            println!("Hi");
            ```

            And so one:

            ```text
            A bit of text here.
            ```

            And so forth:

            ```rust
            println!("Hi");
            ```

            That's all!  Have a nice day!
            "#
        };

        let doc = Doc::from_str(doc_str);
        let expected = Doc::from_str(expected_str);

        let transform = DocTransformRustMarkdownTag::new();

        assert_eq!(transform.transform(&doc).unwrap(), expected);
    }

    #[test]
    fn test_markdown_tag_indent_code_block() {
        let doc_str = indoc! { r#"
            # The crate

            Look a this code:

                println!("Hi");
                if true {
                    println!("There");
                }

            A second one:

                println!("Hi");

                println!("There");

            A second one:

                    println!("Hi");

            That's all!  Have a nice day!
            "#
        };

        let expected_str = indoc! { r#"
            # The crate

            Look a this code:

            ```rust
            println!("Hi");
            if true {
                println!("There");
            }
            ```

            A second one:

            ```rust
            println!("Hi");

            println!("There");
            ```

            A second one:

            ```rust
                println!("Hi");
            ```

            That's all!  Have a nice day!
            "#
        };

        let doc = Doc::from_str(doc_str);
        let expected = Doc::from_str(expected_str);

        let transform = DocTransformRustMarkdownTag::new();

        assert_eq!(transform.transform(&doc).unwrap(), expected);
    }

    #[test]
    fn test_markdown_tag_indent_code_beginning_file() {
        let doc_str = indoc! { r#"
                println!("Hi");

            That's all!  Have a nice day!
            "#
        };

        assert!(doc_str.starts_with("    println!"), "Ensure file starts correctly");

        let expected_str = indoc! { r#"
            ```rust
            println!("Hi");
            ```

            That's all!  Have a nice day!
            "#
        };

        let doc = Doc::from_str(doc_str);
        let expected = Doc::from_str(expected_str);

        let transform = DocTransformRustMarkdownTag::new();

        assert_eq!(transform.transform(&doc).unwrap(), expected);
    }

    #[test]
    fn test_markdown_tag_nested_fenced_block() {
        let doc_str = indoc! { r#"
            ````
            ```
            println!("Hi");
            ```
            ````
            "#
        };

        let expected_str = indoc! { r#"
            ````rust
            ```
            println!("Hi");
            ```
            ````
            "#
        };

        let doc = Doc::from_str(doc_str);
        let expected = Doc::from_str(expected_str);

        let transform = DocTransformRustMarkdownTag::new();

        assert_eq!(transform.transform(&doc).unwrap(), expected);
    }

    #[test]
    fn test_markdown_tag_for_known_code_block_tags() {
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
            let doc_str = format!(
                "Foo:\n```{}\n# This is a comment.\nprintln!(\"#There\");\n```\nEnd\n",
                tag
            );

            let expected_str =
                "Foo:\n```rust\n# This is a comment.\nprintln!(\"#There\");\n```\nEnd\n";

            let doc = Doc::from_str(doc_str);
            let expected = Doc::from_str(expected_str);

            let transform = DocTransformRustMarkdownTag::new();

            assert_eq!(transform.transform(&doc).unwrap(), expected);
        }
    }
}
