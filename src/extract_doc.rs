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

pub fn extract_doc_from_source_file(
    file_path: impl AsRef<Path>,
) -> Result<Option<Doc>, ExtractDocError> {
    let source: String = std::fs::read_to_string(file_path.as_ref())
        .map_err(|_| ExtractDocError::ErrorReadingSourceFile(file_path.as_ref().to_path_buf()))?;

    extract_doc_from_source_str(&source)
}

pub fn extract_doc_from_source_str(source: &str) -> Result<Option<Doc>, ExtractDocError> {
    use syn::{ExprLit, Lit, Meta, MetaNameValue, parse_str};

    let ast: syn::File = parse_str(source).map_err(ExtractDocError::ErrorParsingSourceFile)?;
    let mut lines: Vec<String> = Vec::with_capacity(1024);

    for attr in &ast.attrs {
        if Doc::is_toplevel_doc(attr) {
            if let Meta::NameValue(MetaNameValue {
                value: Expr::Lit(ExprLit { lit: Lit::Str(lstr), .. }),
                ..
            }) = &attr.meta
            {
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
        }
    }

    match lines.is_empty() {
        true => Ok(None),
        false => Ok(Some(Doc { markdown: Markdown::from_lines(&lines) })),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_doc_from_source_str_no_doc() {
        let str = indoc! { r#"
            use std::fs;

            struct Nothing {}
            "#
        };

        assert!(extract_doc_from_source_str(str).unwrap().is_none());
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

        let doc = extract_doc_from_source_str(str).unwrap().unwrap();
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

        let doc = extract_doc_from_source_str(str).unwrap().unwrap();
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

        let doc = extract_doc_from_source_str(str).unwrap().unwrap();
        let lines: Vec<&str> = doc.lines().collect();

        let expected = vec![
            "This is the doc for the crate.  This crate does:",
            "",
            "  1. nothing.",
            "  2. niente.",
        ];

        assert_eq!(lines, expected);
    }
}
