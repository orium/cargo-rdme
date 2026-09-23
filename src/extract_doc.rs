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
    #[error("cannot open included file \"{0}\"")]
    ErrorReadingIncludedFile(PathBuf),
}

pub fn extract_doc_from_source_file(
    file_path: impl AsRef<Path>,
) -> Result<Option<Doc>, ExtractDocError> {
    let file_path = file_path.as_ref();
    let source: String = std::fs::read_to_string(file_path)
        .map_err(|_| ExtractDocError::ErrorReadingSourceFile(file_path.to_path_buf()))?;
    let base_dir = file_path.parent().unwrap_or_else(|| Path::new(""));

    extract_doc_from_source_str(&source, base_dir)
}

fn is_include_str_path(path: &syn::Path) -> bool {
    match path.segments.iter().collect::<Vec<_>>().as_slice() {
        [seg] => seg.ident == "include_str",
        [prefix, seg] => prefix.ident == "std" && seg.ident == "include_str",
        _ => false,
    }
}

pub fn extract_doc_from_source_str(
    source: &str,
    base_dir: impl AsRef<Path>,
) -> Result<Option<Doc>, ExtractDocError> {
    use syn::{ExprLit, ExprMacro, Lit, Meta, MetaNameValue, parse_str};

    let base_dir = base_dir.as_ref();
    let ast: syn::File = parse_str(source).map_err(ExtractDocError::ErrorParsingSourceFile)?;
    let mut lines: Vec<String> = Vec::with_capacity(1024);

    for attr in &ast.attrs {
        if !Doc::is_toplevel_doc(attr) {
            continue;
        }

        let Meta::NameValue(MetaNameValue { value, .. }) = &attr.meta else {
            continue;
        };

        match value {
            Expr::Lit(ExprLit { lit: Lit::Str(lstr), .. }) => {
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
            Expr::Macro(ExprMacro { mac, .. }) if is_include_str_path(&mac.path) => {
                let lstr: syn::LitStr =
                    mac.parse_body().map_err(ExtractDocError::ErrorParsingSourceFile)?;
                let path = base_dir.join(lstr.value());
                let content = std::fs::read_to_string(&path)
                    .map_err(|_| ExtractDocError::ErrorReadingIncludedFile(path))?;

                lines.extend(content.lines().map(ToOwned::to_owned));
            }
            _ => {}
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

        assert!(extract_doc_from_source_str(str, Path::new("")).unwrap().is_none());
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

        let doc = extract_doc_from_source_str(str, Path::new("")).unwrap().unwrap();
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

        let doc = extract_doc_from_source_str(str, Path::new("")).unwrap().unwrap();
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

        let doc = extract_doc_from_source_str(str, Path::new("")).unwrap().unwrap();
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
    fn test_doc_from_source_str_include_str() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(
            dir.path().join("included.md"),
            "# Included\n\nHello from the included file.\n",
        )
        .unwrap();

        let str = indoc! { r#"
            #![doc = include_str!("included.md")]

            struct Nothing {}
            "#
        };

        let doc = extract_doc_from_source_str(str, dir.path()).unwrap().unwrap();
        let lines: Vec<&str> = doc.lines().collect();

        assert_eq!(lines, vec!["# Included", "", "Hello from the included file."]);
    }

    #[test]
    fn test_doc_from_source_str_std_include_str() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("included.md"), "included contents").unwrap();

        let str = r#"#![doc = std::include_str!("included.md")]"#;

        let doc = extract_doc_from_source_str(str, dir.path()).unwrap().unwrap();
        let lines: Vec<&str> = doc.lines().collect();

        assert_eq!(lines, vec!["included contents"]);
    }

    #[test]
    fn test_doc_from_source_str_include_str_relative_to_base_dir() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("docs")).unwrap();
        std::fs::write(dir.path().join("docs/intro.md"), "intro contents").unwrap();

        let str = r#"#![doc = include_str!("docs/intro.md")]"#;

        let doc = extract_doc_from_source_str(str, dir.path()).unwrap().unwrap();
        let lines: Vec<&str> = doc.lines().collect();

        assert_eq!(lines, vec!["intro contents"]);
    }

    #[test]
    fn test_doc_from_source_str_include_str_interleaved() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("mid.md"), "included line 1\nincluded line 2").unwrap();

        let str = indoc! { r#"
            //! Before the include.
            #![doc = include_str!("mid.md")]
            //! After the include.

            struct Nothing {}
            "#
        };

        let doc = extract_doc_from_source_str(str, dir.path()).unwrap().unwrap();
        let lines: Vec<&str> = doc.lines().collect();

        let expected = vec![
            "Before the include.", //
            "included line 1",     //
            "included line 2",     //
            "After the include.",  //
        ];

        assert_eq!(lines, expected);
    }

    #[test]
    fn test_doc_from_source_str_include_str_verbatim() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("verbatim.md"), " leading space kept\n\ttab kept").unwrap();

        let str = r#"#![doc = include_str!("verbatim.md")]"#;

        let doc = extract_doc_from_source_str(str, dir.path()).unwrap().unwrap();
        let lines: Vec<&str> = doc.lines().collect();

        assert_eq!(lines, vec![" leading space kept", "\ttab kept"]);
    }

    #[test]
    fn test_doc_from_source_str_include_str_missing_file() {
        let dir = tempfile::tempdir().unwrap();

        let str = r#"#![doc = include_str!("does_not_exist.md")]"#;

        let err = extract_doc_from_source_str(str, dir.path()).unwrap_err();

        assert!(matches!(err, ExtractDocError::ErrorReadingIncludedFile(_)));
    }

    #[test]
    fn test_doc_from_source_str_non_include_str_macro_skipped() {
        let str = indoc! { r#"
            //! Real doc line.
            #![doc = concat!("a", "b")]

            struct Nothing {}
            "#
        };

        let doc = extract_doc_from_source_str(str, Path::new("")).unwrap().unwrap();
        let lines: Vec<&str> = doc.lines().collect();

        assert_eq!(lines, vec!["Real doc line."]);
    }
}
