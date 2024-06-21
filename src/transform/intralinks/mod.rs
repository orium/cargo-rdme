/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use crate::transform::intralinks::links::{
    markdown_link_iterator, markdown_reference_link_definition_iterator, Link, MarkdownLink,
};
use crate::transform::intralinks::rustdoc::{create_intralink_resolver, IntralinkResolver};
use crate::transform::DocTransform;
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
    #[error("failed to parse rustdoc json file")]
    ParseRustdocError,
    #[error("unsupported rustdoc format version {version} (expected version {expected_version})")]
    UnsupportedRustdocFormatVersion { version: u32, expected_version: u32 },
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
    let inline_links =
        markdown_link_iterator(&doc.markdown).items().filter_map(|link| match link {
            MarkdownLink::Inline { link } => Some(link.link),
            MarkdownLink::Reference { .. } => None,
        });
    let reference_links = markdown_reference_link_definition_iterator(&doc.markdown)
        .items()
        .map(|link_def| link_def.link);

    inline_links.chain(reference_links).any(|link| IntralinkResolver::is_intralink(&link))
}

impl<F> DocTransform for DocTransformIntralinks<F>
where
    F: Fn(&str),
{
    type E = IntralinkError;

    fn transform(&self, doc: &Doc) -> Result<Doc, IntralinkError> {
        // If there are no intralinks we don't want to require users to install the nightly
        // toolchain and to have the performance penalty of running rustdoc.
        if !has_intralinks(doc) {
            return Ok(doc.clone());
        }

        let strip_links = self.config.strip_links.unwrap_or(false);

        // WIP! have a thing that checks if the nightly toolchain is installed

        // We only load intralink resolution information when we need it.
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
            let url = match link.link_fragment() {
                None => url.to_owned(),
                Some(fragment) => format!("{url}#{fragment}"),
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
                match reference_links_to_remove.contains(link.label()) {
                    true => new_doc.push_str(link.text()),
                    false => new_doc.push_str(&link.to_string()),
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

#[allow(clippy::too_many_lines)]
#[cfg(test)]
mod tests {
    /* WIP! Adapt tests
    use super::*;
    use indoc::indoc;

    // WIP! check removed tests: maybe they can exist in some other form

    #[test]
    fn test_documentation_url() {
        let config = IntralinksDocsRsConfig::default();

        let symbols_type: HashMap<ItemPath, SymbolType> =
            [(item_path("crate"), SymbolType::Crate)].into_iter().collect();

        let link = documentation_url(&item_path("crate"), &symbols_type, "foobini", None, &config);
        assert_eq!(link.as_deref(), Some("https://docs.rs/foobini/latest/foobini/"));

        let symbols_type: HashMap<ItemPath, SymbolType> =
            [(item_path("crate::AStruct"), SymbolType::Struct)].into_iter().collect();

        let link = documentation_url(
            &item_path("crate::AStruct"),
            &symbols_type,
            "foobini",
            None,
            &config,
        );
        assert_eq!(
            link.as_deref(),
            Some("https://docs.rs/foobini/latest/foobini/struct.AStruct.html")
        );

        let symbols_type: HashMap<ItemPath, SymbolType> =
            [(item_path("crate::amodule"), SymbolType::Mod)].into_iter().collect();

        let link = documentation_url(
            &item_path("crate::amodule"),
            &symbols_type,
            "foobini",
            None,
            &config,
        );
        assert_eq!(link.as_deref(), Some("https://docs.rs/foobini/latest/foobini/amodule/"));

        let symbols_type: HashMap<ItemPath, SymbolType> =
            [(item_path("::std"), SymbolType::Crate)].into_iter().collect();

        let link = documentation_url(&item_path("::std"), &symbols_type, "foobini", None, &config);
        assert_eq!(link.as_deref(), Some("https://doc.rust-lang.org/stable/std/"));

        let symbols_type: HashMap<ItemPath, SymbolType> =
            [(item_path("::std::collections::HashMap"), SymbolType::Struct)].into_iter().collect();

        let link = documentation_url(
            &item_path("::std::collections::HashMap"),
            &symbols_type,
            "foobini",
            None,
            &config,
        );
        assert_eq!(
            link.as_deref(),
            Some("https://doc.rust-lang.org/stable/std/collections/struct.HashMap.html")
        );

        let symbols_type: HashMap<ItemPath, SymbolType> =
            [(item_path("crate::amodule"), SymbolType::Mod)].into_iter().collect();

        let link = documentation_url(
            &ItemPath::from_string("crate::amodule").unwrap(),
            &symbols_type,
            "foo-bar-mumble",
            None,
            &config,
        );
        assert_eq!(
            link.as_deref(),
            Some("https://docs.rs/foo-bar-mumble/latest/foo_bar_mumble/amodule/")
        );

        let symbols_type: HashMap<ItemPath, SymbolType> =
            [(item_path("crate"), SymbolType::Crate)].into_iter().collect();

        let link = documentation_url(
            &ItemPath::from_string("crate").unwrap(),
            &symbols_type,
            "foo-bar-mumble",
            Some("#enums"),
            &config,
        );
        assert_eq!(
            link.as_deref(),
            Some("https://docs.rs/foo-bar-mumble/latest/foo_bar_mumble/#enums")
        );

        let symbols_type: HashMap<ItemPath, SymbolType> =
            [(item_path("crate::amod"), SymbolType::Mod)].into_iter().collect();

        let link = documentation_url(
            &ItemPath::from_string("crate::amod").unwrap(),
            &symbols_type,
            "foo-bar-mumble",
            Some("#structs"),
            &config,
        );
        assert_eq!(
            link.as_deref(),
            Some("https://docs.rs/foo-bar-mumble/latest/foo_bar_mumble/amod/#structs")
        );

        let symbols_type: HashMap<ItemPath, SymbolType> =
            [(item_path("crate::MyStruct"), SymbolType::Struct)].into_iter().collect();

        let link = documentation_url(
            &ItemPath::from_string("crate::MyStruct").unwrap(),
            &symbols_type,
            "foo-bar-mumble",
            Some("#implementations"),
            &config,
        );
        assert_eq!(
            link.as_deref(),
            Some("https://docs.rs/foo-bar-mumble/latest/foo_bar_mumble/struct.MyStruct.html#implementations")
        );

        let symbols_type: HashMap<ItemPath, SymbolType> = [
            (item_path("crate::mymod::MyStruct"), SymbolType::Struct),
            (
                item_path("crate::mymod::MyStruct::a_method"),
                SymbolType::ImplItem(ImplSymbolType::Method),
            ),
        ]
        .into_iter()
        .collect();

        let link = documentation_url(
            &ItemPath::from_string("crate::mymod::MyStruct::a_method").unwrap(),
            &symbols_type,
            "foo-bar-mumble",
            Some("#thiswillbedropped"),
            &config,
        );
        assert_eq!(
            link.as_deref(),
            Some("https://docs.rs/foo-bar-mumble/latest/foo_bar_mumble/mymod/struct.MyStruct.html#method.a_method")
        );

        let config = IntralinksDocsRsConfig {
            docs_rs_base_url: Some("https://docs.company.rs".to_owned()),
            docs_rs_version: Some("1.0.0".to_owned()),
        };

        let symbols_type: HashMap<ItemPath, SymbolType> =
            [(item_path("crate::Foo"), SymbolType::Struct)].into_iter().collect();

        let link =
            documentation_url(&item_path("crate::Foo"), &symbols_type, "foobini", None, &config);
        assert_eq!(
            link.as_deref(),
            Some("https://docs.company.rs/foobini/1.0.0/foobini/struct.Foo.html")
        );
    }

    #[test]
    fn test_rewrite_markdown_links() {
        let doc = indoc! { r"
            # Foobini

            This [beautiful crate](crate) is cool because it contains [modules](crate::amodule)
            and some other [stuff](https://en.wikipedia.org/wiki/Stuff) as well.

            This link is [broken](crate::broken) and this is [not supported](::foo::bar), but this
            should [wor\\k \[fi\]ne](f\\i\(n\)e).

            Go ahead and check all the [structs in foo](crate::foo#structs) specifically
            [this one](crate::foo::BestStruct).  Also, this is a nice function: [copy](::std::fs::copy).

            [![BestStruct doc](https://example.com/image.png)](crate::foo::BestStruct)
            "
        };

        let symbols_type: HashMap<ItemPath, SymbolType> = [
            (item_path("crate"), SymbolType::Crate),
            (item_path("crate::amodule"), SymbolType::Mod),
            (item_path("crate::foo"), SymbolType::Mod),
            (item_path("crate::foo::BestStruct"), SymbolType::Struct),
            (item_path("::std::fs::copy"), SymbolType::Fn),
        ]
        .into_iter()
        .collect();

        let new_readme = rewrite_markdown_links(
            &Doc::from_str(doc),
            &HashMap::new(),
            &IntralinksConfig::default(),
            &HashSet::new(),
        );
        let expected = indoc! { r"
            # Foobini

            This [beautiful crate](https://docs.rs/foobini/latest/foobini/) is cool because it contains [modules](https://docs.rs/foobini/latest/foobini/amodule/)
            and some other [stuff](https://en.wikipedia.org/wiki/Stuff) as well.

            This link is broken and this is not supported, but this
            should [wor\\k \[fi\]ne](f\\i\(n\)e).

            Go ahead and check all the [structs in foo](https://docs.rs/foobini/latest/foobini/foo/#structs) specifically
            [this one](https://docs.rs/foobini/latest/foobini/foo/struct.BestStruct.html).  Also, this is a nice function: [copy](https://doc.rust-lang.org/stable/std/fs/fn.copy.html).

            [![BestStruct doc](https://example.com/image.png)](https://docs.rs/foobini/latest/foobini/foo/struct.BestStruct.html)
            "
        };

        assert_eq!(new_readme.as_string(), expected);
    }

    #[test]
    fn test_rewrite_markdown_links_strip_links() {
        let doc = indoc! { r"
            # Foobini

            This [beautiful crate](crate) is cool because it contains [modules](crate::amodule)
            and some other [stuff](https://en.wikipedia.org/wiki/Stuff) as well.

            This link is [broken](crate::broken) and this is [not supported](::foo::bar), but this
            should [wor\\k \[fi\]ne](f\\i\(n\)e).

            Go ahead and check all the [structs in foo](crate::foo#structs) specifically
            [this one](crate::foo::BestStruct).  Also, this is a nice function: [copy](::std::fs::copy).

            [![BestStruct doc](https://example.com/image.png)](crate::foo::BestStruct)

            It works with backtricks as well: [modules](`crate::amodule`).  And with
            [reference-style links][ref] (preserving other [references][other]).

            [ref]: crate::foo::AnotherStruct
            [other]: https://en.wikipedia.org/wiki/Reference_(computer_science)
            "
        };

        let symbols_type: HashMap<ItemPath, SymbolType> = [
            (item_path("crate"), SymbolType::Crate),
            (item_path("crate::amodule"), SymbolType::Mod),
            (item_path("crate::foo"), SymbolType::Mod),
            (item_path("crate::foo::BestStruct"), SymbolType::Struct),
            (item_path("crate::foo::AnotherStruct"), SymbolType::Struct),
        ]
        .into_iter()
        .collect();

        let new_readme = rewrite_links(
            &Doc::from_str(doc),
            &HashMap::new(),
            &IntralinksConfig { strip_links: Some(true), ..Default::default() },
        );
        let expected = indoc! { r"
            # Foobini

            This beautiful crate is cool because it contains modules
            and some other [stuff](https://en.wikipedia.org/wiki/Stuff) as well.

            This link is broken and this is not supported, but this
            should [wor\\k \[fi\]ne](f\\i\(n\)e).

            Go ahead and check all the structs in foo specifically
            this one.  Also, this is a nice function: copy.

            ![BestStruct doc](https://example.com/image.png)

            It works with backtricks as well: modules.  And with
            reference-style links (preserving other [references][other]).

            [other]: https://en.wikipedia.org/wiki/Reference_(computer_science)
            "
        };

        assert_eq!(new_readme.as_string(), expected);
    }

    #[test]
    fn test_rewrite_markdown_links_backticked() {
        let doc = indoc! { r"
            # Foobini

            This [beautiful crate](`crate`) is cool because it contains [modules](`crate::amodule`)
            and some other [stuff](https://en.wikipedia.org/wiki/Stuff) as well.

            This link is [broken](`crate::broken`) and this is [not supported](`::foo::bar`), but this
            should [wor\\k \[fi\]ne](f\\i\(n\)e).

            Go ahead and check all the [structs in foo](`crate::foo#structs`) and
            [structs in foo](`crate::foo`#structs) specifically [this one](`crate::foo::BestStruct`).
            Also, this is a nice function: [copy](`::std::fs::copy`).

            [![BestStruct doc](https://example.com/image.png)](`crate::foo::BestStruct`)
            "
        };

        let symbols_type: HashMap<ItemPath, SymbolType> = [
            (item_path("crate"), SymbolType::Crate),
            (item_path("crate::amodule"), SymbolType::Mod),
            (item_path("crate::foo"), SymbolType::Mod),
            (item_path("crate::foo::BestStruct"), SymbolType::Struct),
            (item_path("::std::fs::copy"), SymbolType::Fn),
        ]
        .into_iter()
        .collect();

        let new_readme = rewrite_markdown_links(
            &Doc::from_str(doc),
            &HashMap::new(),
            &IntralinksConfig::default(),
            &HashSet::new(),
        );
        let expected = indoc! { r"
            # Foobini

            This [beautiful crate](https://docs.rs/foobini/latest/foobini/) is cool because it contains [modules](https://docs.rs/foobini/latest/foobini/amodule/)
            and some other [stuff](https://en.wikipedia.org/wiki/Stuff) as well.

            This link is broken and this is not supported, but this
            should [wor\\k \[fi\]ne](f\\i\(n\)e).

            Go ahead and check all the [structs in foo](https://docs.rs/foobini/latest/foobini/foo/#structs) and
            [structs in foo](https://docs.rs/foobini/latest/foobini/foo/#structs) specifically [this one](https://docs.rs/foobini/latest/foobini/foo/struct.BestStruct.html).
            Also, this is a nice function: [copy](https://doc.rust-lang.org/stable/std/fs/fn.copy.html).

            [![BestStruct doc](https://example.com/image.png)](https://docs.rs/foobini/latest/foobini/foo/struct.BestStruct.html)
            "
        };

        assert_eq!(new_readme.as_string(), expected);
    }

    #[test]
    fn test_markdown_reference_definitions() {
        let doc = indoc! { r#"
            # Foobini

            This [beautiful crate] is cool because it contains [modules]
            and some other [stuff] as well.

            This link is [broken] and this is [not supported],
            but this should [wor\\k \[fi\]ne].

            Go ahead and check all the [structs in foo] specifically
            [this one].  Also, this is a nice function: [copy][cp].

            [![BestStruct doc]][BestStruct]

            [beautiful crate]: crate
            [modules]: crate::amodule
            [stuff]: https://en.wikipedia.org/wiki/Stuff
            [broken]: crate::broken
            [not supported]: ::foo::bar
            [wor\\k \[fi\]ne]: f\\i\(n\)e
            [structs in foo]: crate::foo#structs
            [this one]: crate::foo::BestStruct
            [cp]: ::std::fs::copy#examples "A title here"
            [BestStruct doc]: https://example.com/image.png
            [BestStruct]: crate::foo::BestStruct
            "#
        };

        let symbols_type: HashMap<ItemPath, SymbolType> = [
            (item_path("crate"), SymbolType::Crate),
            (item_path("crate::amodule"), SymbolType::Mod),
            (item_path("crate::foo"), SymbolType::Mod),
            (item_path("crate::foo::BestStruct"), SymbolType::Struct),
            (item_path("::std::fs::copy"), SymbolType::Fn),
        ]
        .into_iter()
        .collect();

        let new_readme = rewrite_links(
            &Doc::from_str(doc),
            &HashMap::new(),
            &IntralinksConfig::default(),
        );
        let expected = indoc! { r#"
            # Foobini

            This [beautiful crate] is cool because it contains [modules]
            and some other [stuff] as well.

            This link is broken and this is not supported,
            but this should [wor\\k \[fi\]ne].

            Go ahead and check all the [structs in foo] specifically
            [this one].  Also, this is a nice function: [copy][cp].

            [![BestStruct doc]][BestStruct]

            [beautiful crate]: https://docs.rs/foobini/latest/foobini/
            [modules]: https://docs.rs/foobini/latest/foobini/amodule/
            [stuff]: https://en.wikipedia.org/wiki/Stuff
            [wor\\k \[fi\]ne]: f\\i\(n\)e
            [structs in foo]: https://docs.rs/foobini/latest/foobini/foo/#structs
            [this one]: https://docs.rs/foobini/latest/foobini/foo/struct.BestStruct.html
            [cp]: https://doc.rust-lang.org/stable/std/fs/fn.copy.html#examples "A title here"
            [BestStruct doc]: https://example.com/image.png
            [BestStruct]: https://docs.rs/foobini/latest/foobini/foo/struct.BestStruct.html
            "#
        };

        assert_eq!(new_readme.as_string(), expected);
    }

    #[test]
    fn test_rewrite_markdown_links_removes_links() {
        let doc = indoc! { r"
            # Foobini

            This crate has multiple [modules][mod a].  This link is [broken] and [so is this][null].

            [mod a]: crate::amodule
            [broken]: crate::broken
            [null]: crate::nothing
            "
        };

        let symbols_type: HashMap<ItemPath, SymbolType> =
            [(item_path("crate::amodule"), SymbolType::Mod)].into_iter().collect();

        let new_readme = rewrite_links(
            &Doc::from_str(doc),
            &HashMap::new(),
            &IntralinksConfig::default(),
        );
        let expected = indoc! { r"
            # Foobini

            This crate has multiple [modules][mod a].  This link is broken and so is this.

            [mod a]: https://docs.rs/foobini/latest/foobini/amodule/
            "
        };

        assert_eq!(new_readme.as_string(), expected);
    }

    #[test]
    fn test_markdown_impl_items() {
        let doc = indoc! { r#"
            # Foobini

            This crate has [`Foo::new()`](`crate::Foo::new`), [`Foo::a_method()`](`crate::Foo::a_method`),
            and [`Foo::another_method()`](`crate::Foo::another_method`).

            It also has [`Foo::no_self()`](`crate::Foo::no_self`).  There's also [`Bar::beer()`](`crate::amod::Bar::beer`).

            Struct `Foo` has a [type called `baz`](`crate::Foo::Baz`) and a
            [const called `number`](`crate::Foo::number`).

            We have a function in `FooAlias` [called `hello`](`crate::FooAlias::hello`).

            And in `MyEnum` we have [called `hey`](`crate::MyEnum::hey`).

            And in `MyUnion` we have [called `sup`](`crate::MyUnion::sup`).
            "#
        };

        let symbols_type: HashMap<ItemPath, SymbolType> = [
            (item_path("crate"), SymbolType::Crate),
            (item_path("crate::Foo"), SymbolType::Struct),
            (item_path("crate::Foo::new"), SymbolType::ImplItem(ImplSymbolType::Method)),
            (item_path("crate::Foo::a_method"), SymbolType::ImplItem(ImplSymbolType::Method)),
            (item_path("crate::Foo::another_method"), SymbolType::ImplItem(ImplSymbolType::Method)),
            (item_path("crate::Foo::no_self"), SymbolType::ImplItem(ImplSymbolType::Method)),
            (item_path("crate::amod::Bar"), SymbolType::Struct),
            (item_path("crate::amod::Bar::beer"), SymbolType::ImplItem(ImplSymbolType::Method)),
            (item_path("crate::Foo::Baz"), SymbolType::ImplItem(ImplSymbolType::Type)),
            (item_path("crate::Foo::number"), SymbolType::ImplItem(ImplSymbolType::Const)),
            (item_path("crate::FooAlias"), SymbolType::Type),
            (item_path("crate::FooAlias::hello"), SymbolType::ImplItem(ImplSymbolType::Method)),
            (item_path("crate::MyEnum"), SymbolType::Enum),
            (item_path("crate::MyEnum::hey"), SymbolType::ImplItem(ImplSymbolType::Method)),
            (item_path("crate::MyUnion"), SymbolType::Union),
            (item_path("crate::MyUnion::sup"), SymbolType::ImplItem(ImplSymbolType::Method)),
        ]
        .into_iter()
        .collect();

        let new_readme = rewrite_links(
            &Doc::from_str(doc),
            &HashMap::new(),
            &IntralinksConfig::default(),
        );
        let expected = indoc! { r#"
            # Foobini

            This crate has [`Foo::new()`](https://docs.rs/foobini/latest/foobini/struct.Foo.html#method.new), [`Foo::a_method()`](https://docs.rs/foobini/latest/foobini/struct.Foo.html#method.a_method),
            and [`Foo::another_method()`](https://docs.rs/foobini/latest/foobini/struct.Foo.html#method.another_method).

            It also has [`Foo::no_self()`](https://docs.rs/foobini/latest/foobini/struct.Foo.html#method.no_self).  There's also [`Bar::beer()`](https://docs.rs/foobini/latest/foobini/amod/struct.Bar.html#method.beer).

            Struct `Foo` has a [type called `baz`](https://docs.rs/foobini/latest/foobini/struct.Foo.html#associatedtype.Baz) and a
            [const called `number`](https://docs.rs/foobini/latest/foobini/struct.Foo.html#associatedconstant.number).

            We have a function in `FooAlias` [called `hello`](https://docs.rs/foobini/latest/foobini/type.FooAlias.html#method.hello).

            And in `MyEnum` we have [called `hey`](https://docs.rs/foobini/latest/foobini/enum.MyEnum.html#method.hey).

            And in `MyUnion` we have [called `sup`](https://docs.rs/foobini/latest/foobini/union.MyUnion.html#method.sup).
            "#
        };

        assert_eq!(new_readme.as_string(), expected);
    }

     */
}
