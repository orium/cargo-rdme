/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use crate::transform::utils::{rust_code_block_iterator, ItemOrOther};
use crate::transform::DocTransform;
use crate::Doc;
use itertools::Itertools;
use std::convert::Infallible;

pub struct DocTransformRustRemoveComments;

impl DocTransformRustRemoveComments {
    #[must_use]
    pub fn new() -> DocTransformRustRemoveComments {
        DocTransformRustRemoveComments
    }
}

fn is_line_commented(line: &str) -> bool {
    line.trim_start().starts_with("# ") || line.trim() == "#"
}

fn process_code_block(new_doc_str: &mut String, code_block: &str) {
    let mut first = true;

    for (i, line) in code_block.split('\n').enumerate() {
        // If we have an indent code block and we start with a comment we need to
        // drop any indent whitespace that started this indent block, since
        // pulldown-cmark doesn't consider it part of the code block.
        if i == 0 && is_line_commented(line) {
            while !new_doc_str.ends_with('\n') && !new_doc_str.is_empty() {
                new_doc_str.pop();
            }
        }

        if !is_line_commented(line) {
            if !first {
                new_doc_str.push('\n');
            }

            // Lines starting with `##` are not comments, that is a way to intentionally start a
            // line with `#`.  See https://github.com/rust-lang/rust/pull/41785.
            match line.trim_start().starts_with("##") {
                true => new_doc_str.push_str(&line.replacen("#", "", 1)),
                false => new_doc_str.push_str(line),
            }

            first = false;
        }
    }
}

impl DocTransform for DocTransformRustRemoveComments {
    type E = Infallible;

    fn transform(&self, doc: &Doc) -> Result<Doc, Infallible> {
        let source = doc.lines().join("\n");
        let mut new_doc_str = String::new();

        for item_or_other in rust_code_block_iterator(&source).complete() {
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
    fn test_remove_comments_no_code_block() {
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

        let transform = DocTransformRustRemoveComments::new();

        assert_eq!(transform.transform(&doc).unwrap(), expected);
    }

    #[test]
    fn test_remove_comments_fenced_code_block() {
        let doc_str = indoc! { r#"
            # The crate

            Look a this code:

            ```
            println!("Hi");
            println!("There");
            ```

            A second one:

            ```
            # A comment.
            println!("Hi");
            println!("There");
            ```

            And so one:

            ```
            println!("Hi");
            # A comment.
            println!("There");
            ```

            And so forth:

            ```
            println!("Hi");
            println!("There");
            # A comment.
            ```

            That's all!  Have a nice day!
            "#
        };

        let expected_str = indoc! { r#"
            # The crate

            Look a this code:

            ```
            println!("Hi");
            println!("There");
            ```

            A second one:

            ```
            println!("Hi");
            println!("There");
            ```

            And so one:

            ```
            println!("Hi");
            println!("There");
            ```

            And so forth:

            ```
            println!("Hi");
            println!("There");
            ```

            That's all!  Have a nice day!
            "#
        };

        let doc = Doc::from_str(doc_str);
        let expected = Doc::from_str(expected_str);

        let transform = DocTransformRustRemoveComments::new();

        assert_eq!(transform.transform(&doc).unwrap(), expected);
    }

    #[test]
    fn test_remove_comments_fenced_code_block_starting_with_whitespace() {
        let doc_str = indoc! { r#"
            # The crate

            Look a this code:

              ```
            println!("Hi");
            println!("There");
            ```

            A second one:

              ```
            # A comment.
            println!("Hi");
            println!("There");
            ```

            And so one:

              ```
            println!("Hi");
            # A comment.
            println!("There");
            ```

            And so forth:

              ```
            println!("Hi");
            println!("There");
            # A comment.
            ```

            That's all!  Have a nice day!
            "#
        };

        let expected_str = indoc! { r#"
            # The crate

            Look a this code:

              ```
            println!("Hi");
            println!("There");
            ```

            A second one:

              ```
            println!("Hi");
            println!("There");
            ```

            And so one:

              ```
            println!("Hi");
            println!("There");
            ```

            And so forth:

              ```
            println!("Hi");
            println!("There");
            ```

            That's all!  Have a nice day!
            "#
        };

        let doc = Doc::from_str(doc_str);
        let expected = Doc::from_str(expected_str);

        let transform = DocTransformRustRemoveComments::new();

        assert_eq!(transform.transform(&doc).unwrap(), expected);
    }

    #[test]
    fn test_remove_comments_indent_code_block() {
        let doc_str = indoc! { r#"
            # The crate

            Look a this code:

                println!("Hi");
                println!("There");

            A second one:

                # A comment.
                println!("Hi");
                println!("There");

            And so one:

                println!("Hi");
                # A comment.
                println!("There");

            And so forth:

                println!("Hi");
                println!("There");
                # A comment.

            That's all!  Have a nice day!
            "#
        };

        let expected_str = indoc! { r#"
            # The crate

            Look a this code:

                println!("Hi");
                println!("There");

            A second one:

                println!("Hi");
                println!("There");

            And so one:

                println!("Hi");
                println!("There");

            And so forth:

                println!("Hi");
                println!("There");

            That's all!  Have a nice day!
            "#
        };

        let doc = Doc::from_str(doc_str);
        let expected = Doc::from_str(expected_str);

        let transform = DocTransformRustRemoveComments::new();

        assert_eq!(transform.transform(&doc).unwrap(), expected);
    }

    #[test]
    fn test_remove_comments_indent_code_block_empty_lines() {
        let doc_str = indoc! { r#"
            # The crate

            Look a this code:

                println!("Hi");

                println!("There");

            That's all!  Have a nice day!
            "#
        };

        let expected_str = indoc! { r#"
            # The crate

            Look a this code:

                println!("Hi");

                println!("There");

            That's all!  Have a nice day!
            "#
        };

        let doc = Doc::from_str(doc_str);
        let expected = Doc::from_str(expected_str);

        let transform = DocTransformRustRemoveComments::new();

        assert_eq!(transform.transform(&doc).unwrap(), expected);
    }

    #[test]
    fn test_remove_comments_indent_code_beginning_file_with_comment() {
        let doc_str = indoc! { r#"
                # Comment
                println!("Hi");
                # x
                println!("There");

            That's all!  Have a nice day!
            "#
        };

        assert!(doc_str.starts_with("    #"), "Ensure file starts correctly");

        let expected_str = indoc! { r#"
                println!("Hi");
                println!("There");

            That's all!  Have a nice day!
            "#
        };

        let doc = Doc::from_str(doc_str);
        let expected = Doc::from_str(expected_str);

        let transform = DocTransformRustRemoveComments::new();

        assert_eq!(transform.transform(&doc).unwrap(), expected);
    }

    #[test]
    fn test_remove_comments_indent_code_beginning_file_no_comment() {
        let doc_str = indoc! { r#"
                println!("Hi");
                # x
                println!("There");

            That's all!  Have a nice day!
            "#
        };

        assert!(doc_str.starts_with("    println!"), "Ensure file starts correctly");

        let expected_str = indoc! { r#"
                println!("Hi");
                println!("There");

            That's all!  Have a nice day!
            "#
        };

        let doc = Doc::from_str(doc_str);
        let expected = Doc::from_str(expected_str);

        let transform = DocTransformRustRemoveComments::new();

        assert_eq!(transform.transform(&doc).unwrap(), expected);
    }

    #[test]
    fn test_remove_comments_identify_comment() {
        let doc_str = indoc! { "
            # The crate

            Look a this code:

            ```
            # This is a comment
            #This is not.
            println!(\"There\");
            #
            # ↑ That line is a comment
            #\t
            # ↑ And so is that one.
            ```
            "
        };

        let expected_str = indoc! { r#"
            # The crate

            Look a this code:

            ```
            #This is not.
            println!("There");
            ```
            "#
        };

        let doc = Doc::from_str(doc_str);
        let expected = Doc::from_str(expected_str);

        let transform = DocTransformRustRemoveComments::new();

        assert_eq!(transform.transform(&doc).unwrap(), expected);
    }

    #[test]
    fn test_remove_comments_double_hash_escape_comment() {
        let doc_str = indoc! { r#"
            ```
            if true {
                ## This is not a comment.
                ##And neither is this.
                println!("There");
            }
            ```
            "#
        };

        let expected_str = indoc! { r#"
            ```
            if true {
                # This is not a comment.
                #And neither is this.
                println!("There");
            }
            ```
            "#
        };

        let doc = Doc::from_str(doc_str);
        let expected = Doc::from_str(expected_str);

        let transform = DocTransformRustRemoveComments::new();

        assert_eq!(transform.transform(&doc).unwrap(), expected);
    }

    #[test]
    fn test_remove_comments_for_known_code_block_tags() {
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
            let doc_str = format!("```{}\n# This is a comment.\nprintln!(\"#There\");\n```\n", tag);

            let expected_str = format!("```{}\nprintln!(\"#There\");\n```\n", tag);

            let doc = Doc::from_str(doc_str);
            let expected = Doc::from_str(expected_str);

            let transform = DocTransformRustRemoveComments::new();

            assert_eq!(transform.transform(&doc).unwrap(), expected);
        }
    }

    #[test]
    fn test_remove_comments_for_unknown_code_block_tags_no_change() {
        let tags = ["text", "bash"];

        for tag in tags {
            let doc_str = format!("```{}\n# This is a comment.\nprintln!(\"#There\");\n```\n", tag);
            let doc = Doc::from_str(doc_str);

            let transform = DocTransformRustRemoveComments::new();

            assert_eq!(transform.transform(&doc).unwrap(), doc);
        }
    }

    #[test]
    fn test_remove_comments_nested_fenced_block() {
        let doc_str = indoc! { r#"
            ````
            # Comment 1
            let s = "
            ```
            ";
            # Comment 2
            println!("Hi");
            let s = "
            ```
            ";
            ````
            "#
        };

        let expected_str = indoc! { r#"
            ````
            let s = "
            ```
            ";
            println!("Hi");
            let s = "
            ```
            ";
            ````
            "#
        };

        let doc = Doc::from_str(doc_str);
        let expected = Doc::from_str(expected_str);

        let transform = DocTransformRustRemoveComments::new();

        assert_eq!(transform.transform(&doc).unwrap(), expected);
    }
}
