/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use crate::markdown::Markdown;
use crate::utils::{MarkdownItemIterator, Span};
use itertools::Itertools;
use pulldown_cmark::{CowStr, TagEnd};
use std::fmt;
use std::fmt::Display;
use unicase::UniCase;

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct MarkdownReferenceLinkDefinition {
    pub label: UniCase<String>,
    pub link: Link,
    pub raw_title: Option<String>,
}

impl MarkdownReferenceLinkDefinition {
    fn new(
        label: String,
        link: Link,
        raw_title: Option<String>,
    ) -> MarkdownReferenceLinkDefinition {
        MarkdownReferenceLinkDefinition { label: UniCase::unicode(label), link, raw_title }
    }

    pub fn with_link(&self, link: Link) -> MarkdownReferenceLinkDefinition {
        MarkdownReferenceLinkDefinition {
            label: self.label.clone(),
            link,
            raw_title: self.raw_title.clone(),
        }
    }
}

impl Display for MarkdownReferenceLinkDefinition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let MarkdownReferenceLinkDefinition { label: key, link, raw_title } = self;

        match raw_title {
            None => write!(f, "[{key}]: {link}"),
            Some(title) => write!(f, "[{key}]: {link} {title}"),
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum MarkdownLink {
    Inline { link: MarkdownInlineLink },
    Reference { link: MarkdownReferenceLink },
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub struct MarkdownInlineLink {
    pub text: String,
    pub link: Link,
}

impl MarkdownInlineLink {
    pub fn with_link(&self, link: Link) -> MarkdownInlineLink {
        MarkdownInlineLink { text: self.text.clone(), link }
    }
}

impl Display for MarkdownInlineLink {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}]({})", self.text, self.link)
    }
}

#[derive(Eq, PartialEq, Hash, Clone, Debug)]
pub struct Link {
    pub raw_link: String,
}

impl Link {
    pub fn new(raw_link: String) -> Link {
        Link { raw_link }
    }

    fn split_link_fragment(&self) -> (&str, &str) {
        fn strip_last_backtick(strip_backtick_end: bool, s: &str) -> &str {
            match strip_backtick_end {
                true => s.strip_suffix('`').unwrap_or(s),
                false => s,
            }
        }

        let strip_backtick_end: bool = self.raw_link.starts_with('`');
        let link = self.raw_link.strip_prefix('`').unwrap_or(&self.raw_link);

        match link.find('#') {
            None => (strip_last_backtick(strip_backtick_end, link), ""),
            Some(i) => {
                let (l, f) = link.split_at(i);
                (
                    strip_last_backtick(strip_backtick_end, l),
                    strip_last_backtick(strip_backtick_end, &f[1..]),
                )
            }
        }
    }

    pub fn symbol(&self) -> &str {
        self.split_link_fragment().0
    }

    pub fn link_fragment(&self) -> Option<&str> {
        match self.split_link_fragment().1 {
            "" => None,
            f => Some(f),
        }
    }
}

impl From<String> for Link {
    fn from(raw_link: String) -> Link {
        Link::new(raw_link)
    }
}

impl From<&str> for Link {
    fn from(raw_link: &str) -> Link {
        Link::new(raw_link.to_owned())
    }
}

impl Display for Link {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.raw_link.fmt(f)
    }
}

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum MarkdownReferenceLink {
    /// A reference link like [text][label].
    Normal { text: String, label: UniCase<String> },
    /// A reference link like [label]. Note that the label is also the text.
    Shortcut { text: UniCase<String> },
}

impl MarkdownReferenceLink {
    fn new(text: String, label: String) -> MarkdownReferenceLink {
        MarkdownReferenceLink::Normal { text, label: UniCase::unicode(label) }
    }

    fn new_shortcut(text: String) -> MarkdownReferenceLink {
        MarkdownReferenceLink::Shortcut { text: UniCase::unicode(text) }
    }

    pub fn text(&self) -> &str {
        match self {
            MarkdownReferenceLink::Normal { text, .. } => text,
            MarkdownReferenceLink::Shortcut { text } => text.as_str(),
        }
    }

    pub fn label(&self) -> &UniCase<String> {
        match self {
            MarkdownReferenceLink::Normal { label, .. } => label,
            MarkdownReferenceLink::Shortcut { text } => text,
        }
    }
}

impl Display for MarkdownReferenceLink {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MarkdownReferenceLink::Normal { text, label } => write!(f, "[{text}][{label}]"),
            MarkdownReferenceLink::Shortcut { text: label } => write!(f, "[{label}]"),
        }
    }
}

pub fn markdown_link_iterator(markdown: &Markdown) -> MarkdownItemIterator<MarkdownLink> {
    use pulldown_cmark::{Event, LinkType, Options, Parser, Tag};

    let source = markdown.as_string();

    // We need to define a callback for broken links so that we can see them and strip them.
    let mut broken_link_callback =
        |_| Some((CowStr::Borrowed("fake://link"), CowStr::Borrowed("fake title")));
    let parser = Parser::new_with_broken_link_callback(
        source,
        Options::all(),
        Some(&mut broken_link_callback),
    );

    let mut in_link: Option<LinkType> = None;
    let mut start_text = 0;
    let mut end_text = 0;

    let iter = parser.into_offset_iter().filter_map(move |(event, range)| match event {
        Event::Start(Tag::Link {
            link_type:
                link_type @ (LinkType::Inline
                | LinkType::Reference
                | LinkType::Shortcut
                | LinkType::ReferenceUnknown
                | LinkType::ShortcutUnknown),
            ..
        }) => {
            in_link = Some(link_type);
            start_text = range.start + 1;
            end_text = range.end;
            None
        }
        Event::End(TagEnd::Link) => match in_link {
            Some(LinkType::Inline) => {
                in_link = None;

                let text = source[start_text..end_text].to_owned();
                let link = source[(end_text + 2)..(range.end - 1)].to_owned().into();

                let link = MarkdownLink::Inline { link: MarkdownInlineLink { text, link } };

                Some((range.into(), link))
            }
            Some(LinkType::Reference | LinkType::ReferenceUnknown) => {
                in_link = None;

                let text = source[start_text..end_text].to_owned();
                let label = source[(end_text + 2)..(range.end - 1)].to_owned();

                let link =
                    MarkdownLink::Reference { link: MarkdownReferenceLink::new(text, label) };

                Some((range.into(), link))
            }
            Some(LinkType::Shortcut | LinkType::ShortcutUnknown) => {
                in_link = None;

                let text = source[start_text..end_text].to_owned();

                let link =
                    MarkdownLink::Reference { link: MarkdownReferenceLink::new_shortcut(text) };

                Some((range.into(), link))
            }
            Some(_) | None => None,
        },
        _ => {
            if in_link.is_some() {
                end_text = range.end;
            }

            None
        }
    });

    // Unfortunately we need to collect the iterator here, because the parser references
    // `broken_link_callback` which is in the local stack.
    let collected = iter.collect_vec();

    MarkdownItemIterator::new(source, collected.into_iter())
}

fn parse_raw_reference_link_definition(
    label: &str,
    raw_ref_def: &str,
) -> Option<MarkdownReferenceLinkDefinition> {
    // We need to parse things manually here, because the pulldown-cmark parser escapes the title
    // and the link.  We need the raw version to emit them later.

    let link_and_title = raw_ref_def.get(label.len() + 3..).map(str::trim)?;

    assert_eq!(
        raw_ref_def.get(label.len() + 1..label.len() + 3),
        Some("]:"),
        "This should never happen, but if it does please report it as a bug.",
    );

    let (link, title) = match link_and_title.find(char::is_whitespace) {
        None => (link_and_title, None),
        Some(i) => {
            let (link, title) = link_and_title.split_at(i);
            let title = match title.trim() {
                "" => None,
                title => Some(title),
            };

            (link.trim(), title)
        }
    };

    let link = MarkdownReferenceLinkDefinition::new(
        label.to_owned(),
        link.into(),
        title.map(ToOwned::to_owned),
    );

    Some(link)
}

pub fn markdown_reference_link_definition_iterator(
    markdown: &Markdown,
) -> MarkdownItemIterator<MarkdownReferenceLinkDefinition> {
    use pulldown_cmark::{Options, Parser};

    let source = markdown.as_string();
    let parser = Parser::new_ext(source, Options::all());

    let mut link_defs: Vec<(Span, MarkdownReferenceLinkDefinition)> = parser
        .reference_definitions()
        .iter()
        .filter_map(|(label, ref_def)| {
            let raw_ref_def = &source[ref_def.span.clone()];

            parse_raw_reference_link_definition(label, raw_ref_def)
                .map(|link| (Span::from(ref_def.span.clone()), link))
        })
        .collect_vec();

    // `Parser::reference_definitions()` does not traverse the definitions in order. But
    // `MarkdownItemIterator::complete()` expects the iterated-over spans to be in order.
    link_defs.sort_by_key(|(span, _)| span.clone());

    MarkdownItemIterator::new(source, link_defs.into_iter())
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    #[allow(clippy::too_many_lines)]
    #[test]
    fn test_markdown_link_iterator() {
        let markdown =
            Markdown::from_str("A [some text] [another](http://foo.com), [another][one]");

        let mut iter = markdown_link_iterator(&markdown).items_with_spans();

        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownLink::Reference {
                link: MarkdownReferenceLink::new_shortcut("some text".to_owned()),
            }
        );
        assert_eq!(&markdown.as_string()[start..end], "[some text]");

        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownLink::Inline {
                link: MarkdownInlineLink {
                    text: "another".to_owned(),
                    link: "http://foo.com".into(),
                }
            }
        );
        assert_eq!(&markdown.as_string()[start..end], "[another](http://foo.com)");

        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownLink::Reference {
                link: MarkdownReferenceLink::new("another".to_owned(), "one".to_owned()),
            }
        );
        assert_eq!(&markdown.as_string()[start..end], "[another][one]");

        assert_eq!(iter.next(), None);

        let markdown = Markdown::from_str("[another](http://foo.com)[another][one]");
        let mut iter = markdown_link_iterator(&markdown).items_with_spans();

        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownLink::Inline {
                link: MarkdownInlineLink {
                    text: "another".to_owned(),
                    link: "http://foo.com".into()
                }
            }
        );
        assert_eq!(&markdown.as_string()[start..end], "[another](http://foo.com)");

        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownLink::Reference {
                link: MarkdownReferenceLink::new("another".to_owned(), "one".to_owned()),
            }
        );
        assert_eq!(&markdown.as_string()[start..end], "[another][one]");

        assert_eq!(iter.next(), None);

        let markdown = Markdown::from_str("A [some [text]], [another [text2] (foo)](http://foo.com/foo(bar)), [another [] one][foo[]bar]");
        let mut iter = markdown_link_iterator(&markdown).items_with_spans();

        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownLink::Reference {
                link: MarkdownReferenceLink::new_shortcut("text".to_owned()),
            }
        );
        assert_eq!(&markdown.as_string()[start..end], "[text]");

        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownLink::Reference {
                link: MarkdownReferenceLink::new_shortcut("text2".to_owned()),
            }
        );
        assert_eq!(&markdown.as_string()[start..end], "[text2]");

        assert_eq!(iter.next(), None);

        let markdown = Markdown::from_str(
            "A [some \\]text], [another](http://foo.\\(com\\)), [another\\]][one\\]]",
        );
        let mut iter = markdown_link_iterator(&markdown).items_with_spans();

        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownLink::Reference {
                link: MarkdownReferenceLink::new_shortcut(r"some \]text".to_owned()),
            }
        );
        assert_eq!(&markdown.as_string()[start..end], r"[some \]text]");

        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownLink::Inline {
                link: MarkdownInlineLink {
                    text: "another".to_owned(),
                    link: r"http://foo.\(com\)".into(),
                }
            }
        );
        assert_eq!(&markdown.as_string()[start..end], r"[another](http://foo.\(com\))");

        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownLink::Reference {
                link: MarkdownReferenceLink::new(r"another\]".to_owned(), r"one\]".to_owned()),
            }
        );
        assert_eq!(&markdown.as_string()[start..end], r"[another\]][one\]]");

        assert_eq!(iter.next(), None);

        let markdown = Markdown::from_str("A `this is no link [link](http://foo.com)`");
        let mut iter = markdown_link_iterator(&markdown).items_with_spans();

        assert_eq!(iter.next(), None);

        let markdown = Markdown::from_str("A\n```\nthis is no link [link](http://foo.com)\n```");
        let mut iter = markdown_link_iterator(&markdown).items_with_spans();

        assert_eq!(iter.next(), None);

        let markdown = Markdown::from_str("A [link with `code`!](http://foo.com)!");
        let mut iter = markdown_link_iterator(&markdown).items_with_spans();

        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownLink::Inline {
                link: MarkdownInlineLink {
                    text: "link with `code`!".to_owned(),
                    link: "http://foo.com".into(),
                }
            }
        );
        assert_eq!(&markdown.as_string()[start..end], "[link with `code`!](http://foo.com)");

        assert_eq!(iter.next(), None);

        let doc = indoc! { r#"
            Here a link with [some text] and [another][one].

            [some text]: https://en.wikipedia.org/wiki/Markdown
            [one]: https://en.wikipedia.org/wiki/Markdown
            "#
        };
        let markdown = Markdown::from_str(doc);

        let mut iter = markdown_link_iterator(&markdown).items_with_spans();

        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownLink::Reference {
                link: MarkdownReferenceLink::new_shortcut("some text".to_owned()),
            }
        );
        assert_eq!(&markdown.as_string()[start..end], "[some text]");

        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownLink::Reference {
                link: MarkdownReferenceLink::new("another".to_owned(), "one".to_owned()),
            }
        );
        assert_eq!(&markdown.as_string()[start..end], "[another][one]");

        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_markdown_reference_link_definition_iterator() {
        let doc = indoc! { r#"
            [ref]: https://en.wikipedia.org/wiki/Markdown "This is the title"
            [Another ref]: https://en.wikipedia.org/wiki/Markdown
            [And another one]: https://en.wikipedia.org/wiki/Markdown "Title with &amp;"
            [spaces]:    https://en.wikipedia.org/wiki/Markdown    " Title  with  spaces  "
            "#
        };
        let markdown = Markdown::from_str(doc);

        let mut iter = markdown_reference_link_definition_iterator(&markdown).items_with_spans();

        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownReferenceLinkDefinition::new(
                "ref".to_owned(),
                "https://en.wikipedia.org/wiki/Markdown".into(),
                Some("\"This is the title\"".to_owned()),
            )
        );
        assert_eq!(
            &markdown.as_string()[start..end],
            "[ref]: https://en.wikipedia.org/wiki/Markdown \"This is the title\""
        );

        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownReferenceLinkDefinition::new(
                "Another ref".to_owned(),
                "https://en.wikipedia.org/wiki/Markdown".into(),
                None,
            )
        );
        assert_eq!(
            &markdown.as_string()[start..end],
            "[Another ref]: https://en.wikipedia.org/wiki/Markdown"
        );

        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownReferenceLinkDefinition::new(
                "And another one".to_owned(),
                "https://en.wikipedia.org/wiki/Markdown".into(),
                Some("\"Title with &amp;\"".to_owned()),
            )
        );
        assert_eq!(
            &markdown.as_string()[start..end],
            "[And another one]: https://en.wikipedia.org/wiki/Markdown \"Title with &amp;\""
        );

        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownReferenceLinkDefinition::new(
                "spaces".to_owned(),
                "https://en.wikipedia.org/wiki/Markdown".into(),
                Some("\" Title  with  spaces  \"".to_owned()),
            )
        );
        assert_eq!(
            &markdown.as_string()[start..end],
            "[spaces]:    https://en.wikipedia.org/wiki/Markdown    \" Title  with  spaces  \""
        );

        assert_eq!(iter.next(), None);
    }
}
