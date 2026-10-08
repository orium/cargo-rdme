use crate::Doc;
use crate::markdown::Markdown;
use std::path::{Path, PathBuf};
use syn::Expr;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ExtractDocError {
    #[error("cannot open source file \"{0}\"")]
    ErrorReadingSourceFile(PathBuf),
    #[error("cannot parse source file: {0}")]
    ErrorParsingSourceFile(syn::Error),
}

/// The crate-level documentation extracted from a source file.
pub enum ExtractedDoc {
    /// The source file has no crate-level documentation.
    NoModuleDoc,
    /// A crate-level doc attribute uses a macro (e.g. `#![doc = include_str!(…)]`), whose expansion
    /// this extractor cannot see. The documentation is only complete after rustdoc expands it.
    /// `macro_name` is the name of the first such macro (e.g. `include_str`).
    ContainsDocMacro { macro_name: String },
    /// Documentation built from string-literal doc attributes.
    Literal(Doc),
}

pub fn extract_doc_from_source_file(
    file_path: impl AsRef<Path>,
) -> Result<ExtractedDoc, ExtractDocError> {
    let source: String = std::fs::read_to_string(file_path.as_ref())
        .map_err(|_| ExtractDocError::ErrorReadingSourceFile(file_path.as_ref().to_path_buf()))?;

    extract_doc_from_source_str(&source)
}

pub fn extract_doc_from_source_str(source: &str) -> Result<ExtractedDoc, ExtractDocError> {
    use syn::{ExprLit, Lit, Meta, MetaNameValue, parse_str};

    let ast: syn::File = parse_str(source).map_err(ExtractDocError::ErrorParsingSourceFile)?;
    let mut lines: Vec<String> = Vec::with_capacity(1024);
    let mut doc_macro: Option<String> = None;

    for attr in &ast.attrs {
        if !Doc::is_toplevel_doc(attr) {
            continue;
        }

        match &attr.meta {
            Meta::NameValue(MetaNameValue {
                value: Expr::Lit(ExprLit { lit: Lit::Str(lstr), .. }),
                ..
            }) => {
                let string: String = lstr.value();

                match string.lines().count() {
                    0 => lines.push(String::new()),
                    1 => {
                        let line =
                            string.strip_prefix(' ').map(ToOwned::to_owned).unwrap_or(string);
                        lines.push(line);
                    }

                    // Multiline comment.
                    _ => {
                        fn empty_line(str: &str) -> bool {
                            str.chars().all(char::is_whitespace)
                        }

                        let comment_lines = string
                            .lines()
                            .enumerate()
                            .filter(|(i, l)| !(*i == 0 && empty_line(l)))
                            .map(|(_, l)| l.to_owned());

                        lines.extend(comment_lines);
                    }
                }
            }
            Meta::NameValue(MetaNameValue { value: Expr::Macro(expr_macro), .. }) => {
                doc_macro.get_or_insert_with(|| {
                    expr_macro
                        .mac
                        .path
                        .segments
                        .last()
                        .map_or_else(String::new, |segment| segment.ident.to_string())
                });
            }
            _ => {}
        }
    }

    // A doc macro requires rustdoc to expand it, and rustdoc's expansion includes the string-literal
    // lines too, so it supersedes anything collected here.
    let extracted = match doc_macro {
        Some(macro_name) => ExtractedDoc::ContainsDocMacro { macro_name },
        None if lines.is_empty() => ExtractedDoc::NoModuleDoc,
        None => ExtractedDoc::Literal(Doc { markdown: Markdown::from_lines(&lines) }),
    };

    Ok(extracted)
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    fn literal_doc(source: &str) -> Doc {
        match extract_doc_from_source_str(source).unwrap() {
            ExtractedDoc::Literal(doc) => doc,
            _ => panic!("expected literal documentation"),
        }
    }

    #[test]
    fn test_doc_from_source_str_no_doc() {
        let str = indoc! { r#"
            use std::fs;

            struct Nothing {}
            "#
        };

        assert!(matches!(extract_doc_from_source_str(str).unwrap(), ExtractedDoc::NoModuleDoc));
    }

    #[test]
    fn test_doc_from_source_str_single_line_comment() {
        let str = indoc! { r#"
            #![cfg_attr(not(feature = "std"), no_std)]
            // normal comment

            //! This is the doc for the crate.
            //!This line doesn't start with space.
            //!
            //! And a nice empty line above us.
            //! Also a line ending in "

            struct Nothing {}
            "#
        };

        let doc = literal_doc(str);
        let lines: Vec<&str> = doc.lines().collect();

        let expected = vec![
            "This is the doc for the crate.",
            "This line doesn't start with space.",
            "",
            "And a nice empty line above us.",
            "Also a line ending in \"",
        ];

        assert_eq!(lines, expected);
    }

    #[test]
    fn test_doc_from_source_str_multi_line_comment() {
        let str = indoc! { r#"
            #![cfg_attr(not(feature = "std"), no_std)]
            /* normal comment */

            /*!
            This is the doc for the crate.
             This line start with space.

            And a nice empty line above us.
            */

            struct Nothing {}
            "#
        };

        let doc = literal_doc(str);
        let lines: Vec<&str> = doc.lines().collect();

        let expected = vec![
            "This is the doc for the crate.",
            " This line start with space.",
            "",
            "And a nice empty line above us.",
        ];

        assert_eq!(lines, expected);
    }

    #[test]
    fn test_doc_from_source_str_single_line_keep_indentation() {
        let str = indoc! { r#"
            #![cfg_attr(not(feature = "std"), no_std)]
            // normal comment

            //! This is the doc for the crate.  This crate does:
            //!
            //!   1. nothing.
            //!   2. niente.

            struct Nothing {}
            "#
        };

        let doc = literal_doc(str);
        let lines: Vec<&str> = doc.lines().collect();

        let expected = vec![
            "This is the doc for the crate.  This crate does:",
            "",
            "  1. nothing.",
            "  2. niente.",
        ];

        assert_eq!(lines, expected);
    }

    #[test]
    fn test_doc_from_source_str_no_doc_macro() {
        let str = indoc! { r#"
            //! This is the doc for the crate.

            struct Nothing {}
            "#
        };

        assert!(matches!(extract_doc_from_source_str(str).unwrap(), ExtractedDoc::Literal(_)));
    }

    #[test]
    fn test_doc_from_source_str_include_str_macro() {
        let str = indoc! { r#"
            #![doc = include_str!("README.md")]

            struct Nothing {}
            "#
        };

        assert!(matches!(
            extract_doc_from_source_str(str).unwrap(),
            ExtractedDoc::ContainsDocMacro { macro_name } if macro_name == "include_str"
        ));
    }

    #[test]
    fn test_doc_from_source_str_concat_macro() {
        let str = indoc! { r#"
            //! This is the doc for the crate.
            #![doc = concat!()]

            struct Nothing {}
            "#
        };

        // String-literal lines that sit alongside a doc macro are superseded by rustdoc's
        // expansion, so the macro state wins and the literal lines are dropped here.
        assert!(matches!(
            extract_doc_from_source_str(str).unwrap(),
            ExtractedDoc::ContainsDocMacro { macro_name } if macro_name == "concat"
        ));
    }
}
