use crate::transform::DocTransform;
use crate::transform::intralinks::links::{
    Link, MarkdownInlineLink, MarkdownLink, MarkdownReferenceLink, markdown_link_iterator,
    markdown_reference_link_definition_iterator,
};
pub use crate::transform::intralinks::rustdoc::{
    EXPECTED_RUST_TOOLCHAIN, install_expected_rust_toolchain, is_expected_rust_toolchain_installed,
};
use crate::transform::intralinks::rustdoc::{IntralinkResolver, create_intralink_resolver};
use crate::{Doc, PackageTarget};
use itertools::Itertools;
use std::borrow::Cow;
use std::collections::HashSet;
use std::fmt::Display;
use std::path::PathBuf;
use thiserror::Error;
use unicase::UniCase;

mod links;
mod rustdoc;

#[derive(Error, Debug)]
pub enum IntralinkError {
    #[error("failed to run rustdoc: {error}")]
    RustdocError {
        #[source]
        error: rustdoc_json::BuildError,
    },
    #[error("failed to run rustdoc:\n{stderr}")]
    BuildRustdocError { stderr: String },
    #[error("failed to read rustdoc json file: {io_error}")]
    ReadRustdocError {
        #[source]
        io_error: std::io::Error,
    },
    #[error("failed to parse rustdoc json file: {serde_error}")]
    ParseRustdocError { serde_error: serde_json::Error },
    #[error("unsupported rustdoc format version {version} (expected version {expected_version})")]
    UnsupportedRustdocFormatVersion { version: u32, expected_version: u32 },
    #[error(
        "rust toolchain not installed: {expected}\n\n\
         `cargo-rdme` needs {expected} to do intralink resolution. To install it run:\n\n    \
         rustup toolchain install {expected}\n\n\
         or, equivalently, run `cargo rdme install-rust-toolchain-for-intralinks`."
    )]
    RustToolchainNotInstalled { expected: &'static str },
    #[error("failed to run rustup toolchain: {error}")]
    RustupToolchain { error: rustup_toolchain::Error },
}

#[derive(Default, Debug, PartialEq, Eq, Clone)]
pub struct IntralinksDocsRsConfig {
    pub docs_rs_base_url: Option<String>,
    pub docs_rs_version: Option<String>,
}

#[derive(Default, Debug, PartialEq, Eq, Clone)]
pub struct IntralinksConfig {
    pub docs_rs: IntralinksDocsRsConfig,
    pub strip_links: Option<bool>,
    pub all_features: Option<bool>,
    pub features: Option<Vec<String>>,
    pub no_default_features: Option<bool>,
}

pub struct DocTransformIntralinks<F> {
    package_name: String,
    package_target: PackageTarget,
    workspace_package: Option<String>,
    manifest_path: PathBuf,
    emit_warning: F,
    config: IntralinksConfig,
}

impl<F> DocTransformIntralinks<F>
where
    F: Fn(&str),
{
    pub fn new(
        package_name: impl Into<String>,
        package_target: PackageTarget,
        workspace_package: Option<String>,
        manifest_path: PathBuf,
        emit_warning: F,
        config: Option<IntralinksConfig>,
    ) -> DocTransformIntralinks<F> {
        DocTransformIntralinks {
            package_name: package_name.into(),
            package_target,
            workspace_package,
            manifest_path,
            emit_warning,
            config: config.unwrap_or_default(),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
struct ItemPath<'a> {
    segments: Cow<'a, [String]>,
}

impl<'a> ItemPath<'a> {
    fn new(segments: &'a [String]) -> ItemPath<'a> {
        assert!(!segments.is_empty(), "path item must not be empty");

        ItemPath { segments: Cow::Borrowed(segments) }
    }

    fn add(&self, segment: String) -> ItemPath<'static> {
        let mut segments = self.segments.clone().into_owned();

        segments.push(segment);

        ItemPath { segments: Cow::Owned(segments) }
    }

    fn segments(&self) -> impl Iterator<Item = &str> {
        self.segments.iter().map(String::as_str)
    }

    fn len(&self) -> usize {
        self.segments.len()
    }
}

impl Display for ItemPath<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // TODO Use standard library intersperse() one it stabilizes (https://github.com/rust-lang/rust/issues/79524).
        let iter = Itertools::intersperse(self.segments.iter().map(String::as_str), "::");

        for s in iter {
            f.write_str(s)?;
        }

        Ok(())
    }
}

fn has_intralinks(doc: &Doc) -> bool {
    let link_targets =
        markdown_link_iterator(&doc.markdown).items().filter_map(|link| match link {
            MarkdownLink::Inline { link } => Some(link.link),
            MarkdownLink::Reference { link: MarkdownReferenceLink::Shortcut { text } }
                if is_intralink_shortcut(text.as_str()) =>
            {
                Some(Link::new(text.as_str().to_owned()))
            }
            MarkdownLink::Reference { .. } => None,
        });
    let reference_links = markdown_reference_link_definition_iterator(&doc.markdown)
        .items()
        .map(|link_def| link_def.link);

    link_targets.chain(reference_links).any(|link| IntralinkResolver::is_intralink(&link))
}

impl<F> DocTransform for DocTransformIntralinks<F>
where
    F: Fn(&str),
{
    type E = IntralinkError;

    fn transform(&self, doc: &Doc) -> Result<Doc, IntralinkError> {
        // If there are no intralinks return immediately. No need to run `rustdoc` at all.
        if !has_intralinks(doc) {
            return Ok(doc.clone());
        }

        let strip_links = self.config.strip_links.unwrap_or(false);

        let intralink_resolver: IntralinkResolver<'_> = match strip_links {
            true => {
                // Create an empty resolver, since we are going to strip all intralinks.
                IntralinkResolver::new(self.package_name.as_str(), &self.config.docs_rs)
            }
            false => create_intralink_resolver(
                self.package_name.as_str(),
                &self.package_target,
                self.workspace_package.as_deref(),
                &self.manifest_path,
                &self.config,
            )?,
        };

        let doc = rewrite_links(doc, &intralink_resolver, &self.emit_warning, &self.config);

        Ok(doc)
    }
}

fn rewrite_links(
    doc: &Doc,
    intralink_resolver: &IntralinkResolver,
    emit_warning: &impl Fn(&str),
    config: &IntralinksConfig,
) -> Doc {
    let RewriteReferenceLinksResult { doc, reference_links_to_remove } =
        rewrite_reference_links_definitions(doc, intralink_resolver, emit_warning, config);

    rewrite_markdown_links(
        &doc,
        intralink_resolver,
        emit_warning,
        config,
        &reference_links_to_remove,
    )
}

enum MarkdownLinkAction {
    Link(Link),
    Preserve,
    Strip,
}

fn ensure_backticked(text: &str) -> String {
    let is_backticked = text.len() >= 2 && text.starts_with('`') && text.ends_with('`');

    match is_backticked {
        true => text.to_owned(),
        false => format!("`{text}`"),
    }
}

/// A shortcut link `[text]` looks like a Rust intralink if it is wrapped in backticks
/// (`` [`Foo`] ``), contains a path separator (`[foo::Bar]`), or is a bare identifier (`[Foo]`).
/// The three conditions overlap by design: backticked text typically wraps either an identifier
/// or a path. We check each independently to keep the logic readable.
///
/// Intentionally permissive: in non-strip mode, false positives just miss in the resolver and
/// fall through; in strip mode (where the resolver is empty), this is the only signal we have.
fn is_intralink_shortcut(text: &str) -> bool {
    let backticked = text.len() >= 2 && text.starts_with('`') && text.ends_with('`');
    let has_path_separator = text.contains("::");
    let is_bare_identifier =
        !text.is_empty() && text.chars().all(|c| c.is_alphanumeric() || c == '_');

    backticked || has_path_separator || is_bare_identifier
}

fn resolve_shortcut_intralink(
    link: &MarkdownReferenceLink,
    intralink_resolver: &IntralinkResolver,
    strip_links: bool,
) -> Option<MarkdownLinkAction> {
    let MarkdownReferenceLink::Shortcut { text } = link else {
        return None;
    };

    if strip_links {
        return is_intralink_shortcut(text.as_str()).then_some(MarkdownLinkAction::Strip);
    }

    let candidate = Link::new(text.as_str().to_owned());
    let url = intralink_resolver.resolve_link(&candidate)?;

    let url = match candidate.link_fragment() {
        Some(fragment) if !url.contains('#') => format!("{url}#{fragment}"),
        _ => url.to_owned(),
    };

    Some(MarkdownLinkAction::Link(url.into()))
}

fn markdown_link(
    link: &Link,
    intralink_resolver: &IntralinkResolver,
    emit_warning: &impl Fn(&str),
) -> MarkdownLinkAction {
    assert!(IntralinkResolver::is_intralink(link));

    match intralink_resolver.resolve_link(link) {
        None => {
            emit_warning(&format!("Could not resolve definition of `{}`.", link.symbol()));

            MarkdownLinkAction::Strip
        }
        Some(url) => {
            // For impl items the resolved URL already carries an implicit fragment (e.g.
            // `#method.foo`), so we drop any user-supplied fragment to avoid emitting a URL with
            // two `#`s.
            let url = match link.link_fragment() {
                Some(fragment) if !url.contains('#') => format!("{url}#{fragment}"),
                _ => url.to_owned(),
            };

            MarkdownLinkAction::Link(url.into())
        }
    }
}

fn rewrite_markdown_links(
    doc: &Doc,
    intralink_resolver: &IntralinkResolver,
    emit_warning: &impl Fn(&str),
    config: &IntralinksConfig,
    reference_links_to_remove: &HashSet<UniCase<String>>,
) -> Doc {
    use crate::utils::ItemOrOther;

    let strip_links = config.strip_links.unwrap_or(false);
    let mut new_doc = String::with_capacity(doc.as_string().len() + 1024);

    for item_or_other in markdown_link_iterator(&doc.markdown).complete() {
        match item_or_other {
            ItemOrOther::Item(MarkdownLink::Inline { link: inline_link }) => {
                let markdown_link: MarkdownLinkAction =
                    match IntralinkResolver::is_intralink(&inline_link.link) {
                        true => match strip_links {
                            false => {
                                markdown_link(&inline_link.link, intralink_resolver, emit_warning)
                            }
                            true => MarkdownLinkAction::Strip,
                        },
                        false => MarkdownLinkAction::Preserve,
                    };

                match markdown_link {
                    MarkdownLinkAction::Link(markdown_link) => {
                        new_doc.push_str(&inline_link.with_link(markdown_link).to_string());
                    }
                    MarkdownLinkAction::Preserve => {
                        new_doc.push_str(&inline_link.to_string());
                    }
                    MarkdownLinkAction::Strip => {
                        new_doc.push_str(&inline_link.text);
                    }
                }
            }
            ItemOrOther::Item(MarkdownLink::Reference { link }) => {
                if reference_links_to_remove.contains(link.label()) {
                    new_doc.push_str(link.text());
                } else if let Some(action) =
                    resolve_shortcut_intralink(&link, intralink_resolver, strip_links)
                {
                    // Shortcut intralinks render as code in rustdoc HTML; preserve that by
                    // ensuring the link text is backticked in the README output.
                    let backticked = ensure_backticked(link.text());

                    match action {
                        MarkdownLinkAction::Link(resolved) => {
                            let inline = MarkdownInlineLink { text: backticked, link: resolved };

                            new_doc.push_str(&inline.to_string());
                        }
                        MarkdownLinkAction::Strip => new_doc.push_str(&backticked),
                        MarkdownLinkAction::Preserve => new_doc.push_str(&link.to_string()),
                    }
                } else {
                    new_doc.push_str(&link.to_string());
                }
            }
            ItemOrOther::Other(other) => {
                new_doc.push_str(other);
            }
        }
    }

    Doc::from_str(new_doc)
}

struct RewriteReferenceLinksResult {
    doc: Doc,
    reference_links_to_remove: HashSet<UniCase<String>>,
}

fn rewrite_reference_links_definitions(
    doc: &Doc,
    intralink_resolver: &IntralinkResolver,
    emit_warning: &impl Fn(&str),
    config: &IntralinksConfig,
) -> RewriteReferenceLinksResult {
    use crate::utils::ItemOrOther;
    let mut reference_links_to_remove = HashSet::new();
    let mut new_doc = String::with_capacity(doc.as_string().len() + 1024);
    let mut skip_next_newline = false;
    let strip_links = config.strip_links.unwrap_or(false);

    let iter = markdown_reference_link_definition_iterator(&doc.markdown);

    for item_or_other in iter.complete() {
        match item_or_other {
            ItemOrOther::Item(link_ref_def) => {
                let markdown_link: MarkdownLinkAction =
                    match IntralinkResolver::is_intralink(&link_ref_def.link) {
                        true => match strip_links {
                            false => {
                                markdown_link(&link_ref_def.link, intralink_resolver, emit_warning)
                            }
                            true => MarkdownLinkAction::Strip,
                        },
                        false => MarkdownLinkAction::Preserve,
                    };

                match markdown_link {
                    MarkdownLinkAction::Link(link) => {
                        new_doc.push_str(&link_ref_def.with_link(link).to_string());
                    }
                    MarkdownLinkAction::Preserve => {
                        new_doc.push_str(&link_ref_def.to_string());
                    }
                    MarkdownLinkAction::Strip => {
                        // Do not emit anything to new_doc.
                        reference_links_to_remove.insert(link_ref_def.label);
                        skip_next_newline = true;
                    }
                }
            }
            ItemOrOther::Other(other) => {
                let other = match skip_next_newline {
                    true => {
                        skip_next_newline = false;
                        let next_index = other
                            .chars()
                            .enumerate()
                            .skip_while(|(_, c)| c.is_whitespace() && *c != '\n')
                            .skip(1)
                            .map(|(i, _)| i)
                            .next();

                        next_index.and_then(|i| other.get(i..)).unwrap_or("")
                    }
                    false => other,
                };
                new_doc.push_str(other);
            }
        }
    }

    RewriteReferenceLinksResult { doc: Doc::from_str(new_doc), reference_links_to_remove }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_ensure_backticked() {
        assert_eq!(ensure_backticked("Foo"), "`Foo`");
        assert_eq!(ensure_backticked("foo::Bar"), "`foo::Bar`");
        assert_eq!(ensure_backticked("`Foo`"), "`Foo`");
        assert_eq!(ensure_backticked("`foo::Bar`"), "`foo::Bar`");
        assert_eq!(ensure_backticked(""), "``");
        // A lone backtick is not a matched pair.
        assert_eq!(ensure_backticked("`"), "```");
        // Already-paired empty backticks stay as is.
        assert_eq!(ensure_backticked("``"), "``");
        // Inner backticks are not stripped.
        assert_eq!(ensure_backticked("a`b"), "`a`b`");
    }

    #[test]
    fn test_is_intralink_shortcut() {
        // Bare identifiers.
        assert!(is_intralink_shortcut("Foo"));
        assert!(is_intralink_shortcut("foo"));
        assert!(is_intralink_shortcut("_foo"));
        assert!(is_intralink_shortcut("Foo123"));

        // Path-separated forms.
        assert!(is_intralink_shortcut("foo::Bar"));
        assert!(is_intralink_shortcut("crate::foo::Bar"));
        assert!(is_intralink_shortcut("a b::c")); // whitespace ok if `::` is present.

        // Backticked forms.
        assert!(is_intralink_shortcut("`Foo`"));
        assert!(is_intralink_shortcut("`foo::Bar`"));
        assert!(is_intralink_shortcut("`Foo()`"));
        assert!(is_intralink_shortcut("`Foo!`"));

        // Rejected: not intralink-shaped.
        assert!(!is_intralink_shortcut("some text"));
        assert!(!is_intralink_shortcut("Foo!"));
        assert!(!is_intralink_shortcut("Foo()"));
        assert!(!is_intralink_shortcut(""));
        assert!(!is_intralink_shortcut("`Foo")); // unmatched backtick, no `::`, has backtick.
    }
}
