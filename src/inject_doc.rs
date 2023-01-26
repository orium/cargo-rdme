/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use crate::utils::{ItemOrOther, MarkdownItemIterator, Span};
use crate::{Doc, Readme};
use thiserror::Error;

pub const MARKER_RDME: &str = "<!-- cargo-rdme -->";
const MARKER_RDME_START: &str = "<!-- cargo-rdme start -->";
const MARKER_RDME_END: &str = "<!-- cargo-rdme end -->";

#[derive(PartialEq, Eq, Clone, Debug)]
struct Heading<'a> {
    level: u8,
    text: &'a str,
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum ReadmeLine<'a> {
    Heading(Heading<'a>, Span),
    MarkerCargoRdme(Span),
    MarkerCargoRdmeStart(Span),
    MarkerCargoRdmeEnd(Span),
}

fn readme_line_iterator(readme: &Readme) -> MarkdownItemIterator<ReadmeLine> {
    use pulldown_cmark::{Event, Options, Parser, Tag};

    let source = readme.as_string();
    let parser = Parser::new_ext(source, Options::all());

    let is_line_start =
        |start| start == 0 || source[0..start].chars().rev().find(|&c| c != ' ') == Some('\n');
    let mut depth = 0;

    let iter = parser.into_offset_iter().filter_map(move |(event, range)| match event {
        Event::Start(Tag::Heading(level, ..)) => Some((
            range.clone().into(),
            ReadmeLine::Heading(
                Heading { level: level as u8, text: &source[range.start..range.end] },
                range.into(),
            ),
        )),
        Event::Html(ref html) if is_line_start(range.start) => {
            let trimmed_line = html.strip_suffix('\r').unwrap_or_else(|| html.as_ref()).trim();

            match trimmed_line {
                MARKER_RDME if depth == 0 => {
                    Some((range.clone().into(), ReadmeLine::MarkerCargoRdme(range.into())))
                }
                MARKER_RDME_START if depth == 0 => {
                    depth += 1;
                    Some((range.clone().into(), ReadmeLine::MarkerCargoRdmeStart(range.into())))
                }
                MARKER_RDME_END if depth <= 1 => {
                    depth -= 1;
                    Some((range.clone().into(), ReadmeLine::MarkerCargoRdmeEnd(range.into())))
                }
                MARKER_RDME_START => {
                    depth += 1;
                    None
                }
                MARKER_RDME_END => {
                    depth -= 1;
                    None
                }
                _ => None,
            }
        }
        _ => None,
    });

    MarkdownItemIterator::new(source, iter)
}

fn doc_heading_iterator(doc: &Doc) -> MarkdownItemIterator<Heading> {
    use pulldown_cmark::{Event, Options, Parser, Tag};

    let source = doc.as_string();
    let parser = Parser::new_ext(source, Options::all());

    let iter = parser.into_offset_iter().filter_map(move |(event, range)| match event {
        Event::Start(Tag::Heading(level, ..)) => Some((
            range.clone().into(),
            Heading { level: level as u8, text: &source[range.start..range.end] },
        )),
        _ => None,
    });

    MarkdownItemIterator::new(source, iter)
}

#[derive(Error, Eq, PartialEq, Debug)]
pub enum InjectDocError {
    #[error("unexpected end marker at line {line_number}")]
    UnexpectedMarkerCargoRdmeEnd { line_number: usize },
    #[error("unmatched start marker")]
    UnmatchedMarkerCargoRdmeStart,
}

fn bump_heading_level(doc: &Doc, level_bump: u8) -> Doc {
    let mut new_doc = String::with_capacity(doc.as_string().len() + 256);

    for item in doc_heading_iterator(doc).complete() {
        match item {
            ItemOrOther::Item(Heading { text, .. }) => {
                (0..level_bump).for_each(|_| new_doc.push('#'));
                new_doc.push_str(text);
            }
            ItemOrOther::Other(other) => {
                new_doc.push_str(other);
            }
        }
    }

    Doc::from_str(new_doc)
}

pub struct NewReadme {
    pub readme: Readme,
    /// Weather the README had a cargo-rdme marker or not.
    pub had_marker: bool,
}

pub fn inject_doc_in_readme(readme: &Readme, doc: &Doc) -> Result<NewReadme, InjectDocError> {
    fn inject(new_readme: &mut String, doc: &Doc) {
        new_readme.push_str(MARKER_RDME_START);
        new_readme.push_str("\n\n");
        doc.lines().for_each(|line| {
            new_readme.push_str(line);
            new_readme.push('\n');
        });
        new_readme.push('\n');
        new_readme.push_str(MARKER_RDME_END);
        new_readme.push('\n');
    }

    let mut new_readme: String =
        String::with_capacity(readme.as_string().len() + doc.as_string().len() + 1024);
    let mut inside_markers = false;
    let mut last_heading_level: u8 = 0;
    let mut had_marker = false;

    for item in readme_line_iterator(readme).complete() {
        match (inside_markers, item) {
            (true, ItemOrOther::Item(ReadmeLine::MarkerCargoRdmeEnd(_))) => {
                inside_markers = false;
            }
            (true, _) => (),

            (false, ItemOrOther::Item(ReadmeLine::MarkerCargoRdmeEnd(span))) => {
                let line_number =
                    1 + readme.as_string()[0..span.start].chars().filter(|&c| c == '\n').count();

                return Err(InjectDocError::UnexpectedMarkerCargoRdmeEnd { line_number });
            }
            (false, ItemOrOther::Item(ReadmeLine::Heading(Heading { level, text }, _))) => {
                new_readme.push_str(text);
                last_heading_level = level;
            }
            (false, ItemOrOther::Other(other)) => new_readme.push_str(other),
            (false, ItemOrOther::Item(ReadmeLine::MarkerCargoRdme(_))) => {
                let doc = bump_heading_level(doc, last_heading_level);
                inject(&mut new_readme, &doc);
                had_marker = true;
            }
            (false, ItemOrOther::Item(ReadmeLine::MarkerCargoRdmeStart(_))) => {
                let doc = bump_heading_level(doc, last_heading_level);
                inject(&mut new_readme, &doc);
                inside_markers = true;
                had_marker = true;
            }
        }
    }

    match inside_markers {
        true => Err(InjectDocError::UnmatchedMarkerCargoRdmeStart),
        false => {
            let new_readme = NewReadme { readme: Readme::from_str(new_readme), had_marker };

            Ok(new_readme)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_readme_line_iterator() {
        let str = indoc! { "
            marker test <!-- cargo-rdme -->.
             Starting with whitespace.

            <!-- cargo-rdme start -->
            <!-- cargo-rdme end -->
            <!-- cargo-rdme -->
            <!-- cargo-rdme end --> <- Does not count.
             <!-- cargo-rdme start -->
            <!-- cargo-rdme end -->\r
            <!-- cargo-rdme start -->
            <!-- cargo-rdme end --> \r
            <!-- cargo-rdme start -->
            <!-- cargo-rdme end --> "
        };

        let readme = Readme::from_str(str);
        let mut iter = readme_line_iterator(&readme).items();

        // TODO Replace by `assert_matches!()` once https://github.com/rust-lang/rust/issues/82775
        // stabilizes.
        assert!(matches!(iter.next(), Some(ReadmeLine::MarkerCargoRdmeStart(_))));
        assert!(matches!(iter.next(), Some(ReadmeLine::MarkerCargoRdmeEnd(_))));
        assert!(matches!(iter.next(), Some(ReadmeLine::MarkerCargoRdme(_))));
        assert!(matches!(iter.next(), Some(ReadmeLine::MarkerCargoRdmeStart(_))));
        assert!(matches!(iter.next(), Some(ReadmeLine::MarkerCargoRdmeEnd(_))));
        assert!(matches!(iter.next(), Some(ReadmeLine::MarkerCargoRdmeStart(_))));
        assert!(matches!(iter.next(), Some(ReadmeLine::MarkerCargoRdmeEnd(_))));
        assert!(matches!(iter.next(), Some(ReadmeLine::MarkerCargoRdmeStart(_))));
        assert!(matches!(iter.next(), Some(ReadmeLine::MarkerCargoRdmeEnd(_))));
        assert!(matches!(iter.next(), None));
    }

    #[test]
    fn test_readme_line_iterator_nested() {
        let str = indoc! { "
            A
            <!-- cargo-rdme start -->
            B
            <!-- cargo-rdme -->
            C
            <!-- cargo-rdme start -->
            D
            <!-- cargo-rdme end -->
            E
            <!-- cargo-rdme end -->
            F"
        };

        let readme = Readme::from_str(str);
        let mut iter = readme_line_iterator(&readme).items();

        // TODO Replace by `assert_matches!()` once https://github.com/rust-lang/rust/issues/82775
        // stabilizes.
        assert!(matches!(iter.next(), Some(ReadmeLine::MarkerCargoRdmeStart(_))));
        assert!(matches!(iter.next(), Some(ReadmeLine::MarkerCargoRdmeEnd(_))));
        assert!(matches!(iter.next(), None));
    }

    #[test]
    fn test_inject_doc_single_marker() {
        let readme_str = indoc! { r#"
            This is a really nice crate.

            <!-- cargo-rdme -->

            Hope you enjoy!
            "#
        };
        let doc_str = indoc! { r#"
            # The crate

            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
            incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
            exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
            "#
        };

        let expected = indoc! { r#"
            This is a really nice crate.

            <!-- cargo-rdme start -->

            # The crate

            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
            incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
            exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

            <!-- cargo-rdme end -->

            Hope you enjoy!
            "#
        };

        let readme = Readme::from_str(readme_str);
        let doc = Doc::from_str(doc_str);

        let new_readme = inject_doc_in_readme(&readme, &doc).unwrap();

        assert_eq!(new_readme.readme.markdown.as_string(), expected);
        assert!(new_readme.had_marker);
    }

    #[test]
    fn test_inject_doc_start_end_marker() {
        let readme_str = indoc! { r#"
            This is a really nice crate.

            <!-- cargo-rdme start -->

            Li Europan lingues es membres del sam familie. Lor separat existentie es un myth.
            Por scientie, musica, sport etc, litot Europa usa li sam vocabular.

            <!-- cargo-rdme end -->

            Hope you enjoy!
            "#
        };
        let doc_str = indoc! { r#"
            # The crate

            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
            incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
            exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
            "#
        };

        let expected = indoc! { r#"
            This is a really nice crate.

            <!-- cargo-rdme start -->

            # The crate

            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
            incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
            exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

            <!-- cargo-rdme end -->

            Hope you enjoy!
            "#
        };

        let readme = Readme::from_str(readme_str);
        let doc = Doc::from_str(doc_str);

        let new_readme = inject_doc_in_readme(&readme, &doc).unwrap();

        assert_eq!(new_readme.readme.markdown.as_string(), expected);
        assert!(new_readme.had_marker);
    }

    #[test]
    fn test_inject_doc_unmatched_start_marker() {
        let readme_str = indoc! { r#"
            This is a really nice crate.

            <!-- cargo-rdme -->

            <!-- cargo-rdme start -->

            Hope you enjoy!
            "#
        };
        let doc_str = indoc! { r#"
            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
            incididunt ut labore et dolore magna aliqua.
            "#
        };

        let readme = Readme::from_str(readme_str);
        let doc = Doc::from_str(doc_str);

        let result = inject_doc_in_readme(&readme, &doc);

        assert_eq!(result.err(), Some(InjectDocError::UnmatchedMarkerCargoRdmeStart));
    }

    #[test]
    fn test_inject_doc_unexpected_end_marker() {
        let readme_str = indoc! { r#"
            This is a really nice crate.

            <!-- cargo-rdme -->

            <!-- cargo-rdme end -->

            Hope you enjoy!
            "#
        };
        let doc_str = indoc! { r#"
            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
            incididunt ut labore et dolore magna aliqua.
            "#
        };

        let readme = Readme::from_str(readme_str);
        let doc = Doc::from_str(doc_str);

        let result = inject_doc_in_readme(&readme, &doc);

        assert_eq!(
            result.err(),
            Some(InjectDocError::UnexpectedMarkerCargoRdmeEnd { line_number: 5 })
        );
    }

    #[test]
    fn test_bump_heading_level() {
        let doc_str = indoc! { r#"
            # Foo
            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
            incididunt ut labore et dolore magna aliqua.

            ## Bar
            Aenean dictum in nisi eu rutrum. Suspendisse vulputate tristique turpis eu vestibulum.
            "#
        };
        let doc = Doc::from_str(doc_str);

        let new_readme = bump_heading_level(&doc, 0);

        assert_eq!(new_readme.markdown.as_string(), doc_str);

        let expected = indoc! { r#"
            ### Foo
            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
            incididunt ut labore et dolore magna aliqua.

            #### Bar
            Aenean dictum in nisi eu rutrum. Suspendisse vulputate tristique turpis eu vestibulum.
            "#
        };

        let new_readme = bump_heading_level(&doc, 2);

        assert_eq!(new_readme.markdown.as_string(), expected);
    }

    #[test]
    fn test_inject_doc_bump_heading_level() {
        let readme_str = indoc! { r#"
            # The crate

            This is a really nice crate.

            <!-- cargo-rdme -->

            Hope you enjoy!
            "#
        };
        let doc_str = indoc! { r#"
            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
            incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
            exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

            # Foo

            Aenean dictum in nisi eu rutrum. Suspendisse vulputate tristique turpis eu vestibulum.
            "#
        };

        let expected = indoc! { r#"
            # The crate

            This is a really nice crate.

            <!-- cargo-rdme start -->

            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
            incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
            exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

            ## Foo

            Aenean dictum in nisi eu rutrum. Suspendisse vulputate tristique turpis eu vestibulum.

            <!-- cargo-rdme end -->

            Hope you enjoy!
            "#
        };

        let readme = Readme::from_str(readme_str);
        let doc = Doc::from_str(doc_str);

        let new_readme = inject_doc_in_readme(&readme, &doc).unwrap();

        assert_eq!(new_readme.readme.markdown.as_string(), expected);
        assert!(new_readme.had_marker);
    }

    #[test]
    fn test_inject_doc_bump_heading_level_ignore_within_markers() {
        let readme_str = indoc! { r#"
            # The crate

            This is a really nice crate.

            <!-- cargo-rdme start -->

            ### The crate

            Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
            incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud
            exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

            <!-- cargo-rdme end -->

            Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus.

            <!-- cargo-rdme -->

            Hope you enjoy!
            "#
        };
        let doc_str = indoc! { r#"
            # Foo

            Aenean dictum in nisi eu rutrum. Suspendisse vulputate tristique turpis eu vestibulum.
            "#
        };

        let expected = indoc! { r#"
            # The crate

            This is a really nice crate.

            <!-- cargo-rdme start -->

            ## Foo

            Aenean dictum in nisi eu rutrum. Suspendisse vulputate tristique turpis eu vestibulum.

            <!-- cargo-rdme end -->

            Orci varius natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus.

            <!-- cargo-rdme start -->

            ## Foo

            Aenean dictum in nisi eu rutrum. Suspendisse vulputate tristique turpis eu vestibulum.

            <!-- cargo-rdme end -->

            Hope you enjoy!
            "#
        };

        let readme = Readme::from_str(readme_str);
        let doc = Doc::from_str(doc_str);

        let new_readme = inject_doc_in_readme(&readme, &doc).unwrap();

        assert_eq!(new_readme.readme.markdown.as_string(), expected);
        assert!(new_readme.had_marker);
    }

    #[test]
    fn test_inject_doc_bump_heading_level_ignore_code_blocks() {
        let readme_str = indoc! { r#"
            # The crate

            This is a really nice crate.
            You should try it!

            ```
            ### This is code
            ```

            <!-- cargo-rdme -->
            "#
        };
        let doc_str = indoc! { r#"
            # Foo

            Aenean dictum in nisi eu rutrum. Suspendisse vulputate tristique turpis eu vestibulum.
            "#
        };

        let expected = indoc! { r#"
            # The crate

            This is a really nice crate.
            You should try it!

            ```
            ### This is code
            ```

            <!-- cargo-rdme start -->

            ## Foo

            Aenean dictum in nisi eu rutrum. Suspendisse vulputate tristique turpis eu vestibulum.

            <!-- cargo-rdme end -->
            "#
        };

        let readme = Readme::from_str(readme_str);
        let doc = Doc::from_str(doc_str);

        let new_readme = inject_doc_in_readme(&readme, &doc).unwrap();

        assert_eq!(new_readme.readme.markdown.as_string(), expected);
        assert!(new_readme.had_marker);
    }
}
