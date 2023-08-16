/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use crate::transform::intralinks::links::{
    markdown_link_iterator, markdown_reference_link_definition_iterator, Link, MarkdownLink,
};
use crate::transform::DocTransform;
use crate::{Doc, PackageTarget};
use itertools::Itertools;
use rustdoc_types::{ItemEnum, ItemKind, ItemSummary, StructKind};
use std::borrow::Cow;
use std::collections::{HashMap, HashSet};
use syn::__private::str;
use thiserror::Error;
use unicase::UniCase;

mod links;

#[derive(Error, Debug)]
pub enum IntralinkError {
    #[error("IO error: {0}")]
    IOError(std::io::Error),
    #[error("failed to load standard library: {0}")]
    LoadStdLibError(String),
}

impl From<std::io::Error> for IntralinkError {
    fn from(err: std::io::Error) -> Self {
        IntralinkError::IOError(err)
    }
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
    crate_name: String,
    package_target: PackageTarget,
    workspace_package: Option<String>,
    emit_warning: F,
    config: IntralinksConfig,
}

impl<F> DocTransformIntralinks<F>
where
    F: Fn(&str),
{
    pub fn new(
        crate_name: impl Into<String>,
        package_target: PackageTarget,
        workspace_package: Option<String>,
        emit_warning: F,
        config: Option<IntralinksConfig>,
    ) -> DocTransformIntralinks<F> {
        DocTransformIntralinks {
            crate_name: crate_name.into(),
            package_target,
            workspace_package,
            emit_warning,
            config: config.unwrap_or_default(),
        }
    }
}

mod rustdoc {
    use rustdoc_types::*;
    use std::collections::HashMap;
    use std::path::Path;

    // WIP! error handling: return own enum
    pub fn crate_from_file(path: &Path) -> Crate {
        let json = std::fs::read_to_string(path).expect("WIP!");
        serde_json::from_str(&json).expect("WIP!")
    }

    pub fn crate_rustdoc_intralinks(c: &Crate) -> &HashMap<String, Id> {
        &c.index.get(&c.root).expect("root id not present in index").links
    }
}

#[derive(PartialEq, Eq, Hash, Clone)]
struct ItemPath<'a> {
    segments: Cow<'a, [String]>,
}

impl<'a> ItemPath<'a> {
    fn new(segments: &'a [String]) -> ItemPath<'a> {
        assert!(segments.len() > 0, "path item must not be empty");

        ItemPath { segments: Cow::Borrowed(segments) }
    }

    fn push(&self, segment: String) -> ItemPath<'static> {
        let mut segments = self.segments.clone().into_owned();

        segments.push(segment);

        ItemPath { segments: Cow::Owned(segments) }
    }

    fn parent(&'a self) -> Option<ItemPath<'a>> {
        match self.segments.len() {
            0 => unreachable!("this should never happen"),
            1 => None,
            len => Some(ItemPath::new(&self.segments[0..len - 1])),
        }
    }

    fn name(&self) -> &str {
        self.segments.last().expect("segments should not be empty")
    }
}

// WIP! name
struct ItemInfo {
    crate_id: u32,
    kind: ItemKind,
}

impl From<&ItemSummary> for ItemInfo {
    fn from(item_summary: &ItemSummary) -> Self {
        ItemInfo { crate_id: item_summary.crate_id, kind: item_summary.kind.clone() }
    }
}

fn path_of_struct_with_field<'a>(
    rustdoc_crate: &'a rustdoc_types::Crate,
    field_id: &rustdoc_types::Id,
) -> Option<ItemPath<'a>> {
    use rustdoc_types::Struct;

    let struct_item = rustdoc_crate.index.values().find(|item| match &item.inner {
        ItemEnum::Struct(Struct { kind: StructKind::Plain { fields, .. }, .. }) => {
            fields.contains(field_id)
        }
        _ => false,
    })?;

    let struct_summay = rustdoc_crate.paths.get(&struct_item.id)?;

    Some(ItemPath::new(&struct_summay.path))
}

// WIP! name
fn foo<'a>(
    rustdoc_crate: &'a rustdoc_types::Crate,
    id: &rustdoc_types::Id,
) -> Option<(ItemPath<'a>, ItemInfo)> {
    let item = rustdoc_crate.index.get(id)?;

    match item.inner {
        ItemEnum::StructField(_) => {
            let item_info = ItemInfo { crate_id: item.crate_id, kind: ItemKind::StructField };
            let name = item.name.as_ref()?;
            let parent_path = path_of_struct_with_field(rustdoc_crate, id)?;
            let path = parent_path.push(name.clone());

            Some((path, item_info))
        }
        ItemEnum::Module(_)
        | ItemEnum::ExternCrate { .. }
        | ItemEnum::Import(_)
        | ItemEnum::Union(_)
        | ItemEnum::Struct(_)
        | ItemEnum::Enum(_)
        | ItemEnum::Variant(_)
        | ItemEnum::Function(_)
        | ItemEnum::Trait(_)
        | ItemEnum::TraitAlias(_)
        | ItemEnum::Impl(_)
        | ItemEnum::TypeAlias(_)
        | ItemEnum::OpaqueTy(_)
        | ItemEnum::Constant(_)
        | ItemEnum::Static(_)
        | ItemEnum::ForeignType
        | ItemEnum::Macro(_)
        | ItemEnum::ProcMacro(_)
        | ItemEnum::Primitive(_)
        | ItemEnum::AssocConst { .. }
        | ItemEnum::AssocType { .. } => todo!("WIP!"),
    }
}

impl<F> DocTransform for DocTransformIntralinks<F>
where
    F: Fn(&str),
{
    type E = IntralinkError;

    fn transform(&self, doc: &Doc) -> Result<Doc, IntralinkError> {
        // WIP! have a thing that checks if the nightly toolchain is installed.

        // WIP! do not run intralinks if there's none.

        // WIP! we probably want to be able to configure which feature to have enabled when generating the code
        // or at least never fail on warnings

        // WIP! make sure we print errors we got from rustdoc, but not warnings (probably)
        //      maybe we can do with --cap-lints error ?

        let mut link_url_resolver: LinkUrlResolver =
            LinkUrlResolver::new(self.crate_name.clone(), self.config.docs_rs.clone());

        // We only load symbols type information when we need them.
        if !self.config.strip_links.unwrap_or(false) {
            let target = match &self.package_target {
                PackageTarget::Bin { name } => rustdoc_json::PackageTarget::Bin(name.clone()),
                PackageTarget::Lib => rustdoc_json::PackageTarget::Lib,
            };

            // WIP! check version `format_version` before trying to parse for real

            // WIP! set temporary target dir with target_dir?
            // silent(self, silent: bool)
            // quiet(self, quiet: bool)
            let rustdoc_json_path = {
                let mut builder = rustdoc_json::Builder::default()
                    // TODO Use stable when this stabilizes (https://github.com/rust-lang/rust/issues/76578).
                    .toolchain("nightly")
                    // WIP! get manifest_path from somewhere
                    .manifest_path("Cargo.toml")
                    .document_private_items(true)
                    // WIP! Should we parameterize the features thing?
                    // .all_features(true)
                    .package_target(target);

                if let Some(package) = self.workspace_package.as_ref() {
                    builder = builder.package(package);
                }

                builder.build()
                    // WIP! proper error handling
                    .expect("WIP!")
            };

            // WIP! make sure we can display error if they happen.

            eprintln!("WIP! GREPME {}", rustdoc_json_path.display());

            let rustdoc_crate = rustdoc::crate_from_file(&rustdoc_json_path);

            // eprintln!("WIP! GREPME {:#?}", rustdoc_crate);

            let links_items_id = rustdoc::crate_rustdoc_intralinks(&rustdoc_crate);

            let mut path_item_info: HashMap<ItemPath<'_>, ItemInfo> =
                HashMap::with_capacity(links_items_id.len());

            for item_summary in rustdoc_crate.paths.values() {
                let item_path = ItemPath::new(&item_summary.path);
                let item_info =
                    ItemInfo { crate_id: item_summary.crate_id, kind: item_summary.kind.clone() };

                path_item_info.insert(item_path, item_info);
            }

            // WIP! We don't need to find the specific struct this way.  lets pass everything to
            // the resolver.
            for (link, item_id) in links_items_id {
                eprintln!("GREPME link {:?} itemid {:?}", link, item_id);

                if !rustdoc_crate.paths.contains_key(item_id) {
                    let (item_path, item_info) =
                        foo(&rustdoc_crate, &item_id).expect("could not find item info");

                    path_item_info.insert(item_path, item_info);
                }
            }

            for (link, item_id) in links_items_id {
                // WIP! do not compute this again (see for loops above)
                let (item_path, _): (ItemPath<'_>, ItemInfo) =
                    match rustdoc_crate.paths.get(item_id) {
                        None => foo(&rustdoc_crate, item_id).expect("could not find item info"),
                        Some(item_summary) => {
                            let item_path = ItemPath::new(item_summary.path.as_slice());
                            (item_path, item_summary.into())
                        }
                    };

                link_url_resolver.add(
                    Link::new(link.clone()),
                    item_path,
                    &path_item_info,
                    &rustdoc_crate.external_crates,
                )
            }
        }

        let doc = rewrite_links(doc, &link_url_resolver, &self.emit_warning, &self.config);

        Ok(doc)
    }
}

// WIP! reference links
// WIP! impl items
// WIP! external links like stdlib

struct LinkUrlResolver {
    link_url: HashMap<Link, String>,
    config: IntralinksDocsRsConfig,
    crate_name: String,
}

impl LinkUrlResolver {
    fn new(crate_name: String, config: IntralinksDocsRsConfig) -> LinkUrlResolver {
        LinkUrlResolver { link_url: HashMap::new(), crate_name, config }
    }

    // WIP! name
    fn url_last_component(
        kind: ItemKind,
        name: &str,
        parent_kind: Option<ItemKind>,
        parent_name: Option<&str>,
    ) -> String {
        match kind {
            // WIP! about about fields inside a union, enum, struct, functions in a trait etc?
            ItemKind::Module => format!("{name}/"),
            ItemKind::ExternCrate => todo!("WIP!"), //
            ItemKind::Import => todo!("WIP!"),      //
            ItemKind::Struct => format!("struct.{name}.html"),
            ItemKind::StructField => {
                let parent_kind = parent_kind.expect("this item kind needs to have a parent");
                let parent_name = parent_name.expect("this item kind needs to have a parent");
                let parent_last_component =
                    LinkUrlResolver::url_last_component(parent_kind, parent_name, None, None);

                format!("{}#structfield.{name}", parent_last_component)
            }
            ItemKind::Union => format!("union.{name}.html"),
            ItemKind::Enum => format!("enum.{name}.html"),
            ItemKind::Variant => todo!("WIP!"), //
            ItemKind::Function => format!("fn.{name}.html"),
            ItemKind::TypeAlias => format!("type.{name}.html"),
            ItemKind::OpaqueTy => todo!("WIP!"), //
            ItemKind::Constant => format!("const.{name}.html"),
            ItemKind::Trait => format!("trait.{name}.html"),
            ItemKind::TraitAlias => todo!("WIP!"), //
            ItemKind::Impl => todo!("WIP!"),       //
            ItemKind::Static => format!("static.{name}.html"),
            ItemKind::ForeignType => todo!("WIP!"), //
            ItemKind::Macro => format!("macro.{name}.html"),
            ItemKind::ProcAttribute => todo!("WIP!"), //
            ItemKind::ProcDerive => todo!("WIP!"),    //
            ItemKind::AssocConst => todo!("WIP!"),    //
            ItemKind::AssocType => todo!("WIP!"),     //
            ItemKind::Primitive => format!("primitive.{name}.html"),
            ItemKind::Keyword => todo!("WIP!"), //
        }
    }

    // WIP! name
    fn is_nested_item_kind(kind: ItemKind) -> bool {
        match kind {
            // WIP! about about fields inside a union, enum, struct, functions in a trait etc?
            ItemKind::Module => false,
            ItemKind::ExternCrate => todo!("WIP!"), //
            ItemKind::Import => todo!("WIP!"),      //
            ItemKind::Struct => false,
            ItemKind::StructField => true,
            ItemKind::Union => false,
            ItemKind::Enum => false,
            ItemKind::Variant => todo!("WIP!"), //
            ItemKind::Function => false,
            ItemKind::TypeAlias => false,
            ItemKind::OpaqueTy => todo!("WIP!"), //
            ItemKind::Constant => false,
            ItemKind::Trait => false,
            ItemKind::TraitAlias => todo!("WIP!"), //
            ItemKind::Impl => todo!("WIP!"),       //
            ItemKind::Static => false,
            ItemKind::ForeignType => todo!("WIP!"), //
            ItemKind::Macro => false,
            ItemKind::ProcAttribute => todo!("WIP!"), //
            ItemKind::ProcDerive => todo!("WIP!"),    //
            ItemKind::AssocConst => todo!("WIP!"),    //
            ItemKind::AssocType => todo!("WIP!"),     //
            ItemKind::Primitive => false,
            ItemKind::Keyword => todo!("WIP!"), //
        }
    }

    fn add(
        &mut self,
        link: Link,
        item_path: ItemPath<'_>,
        links_items_id: &HashMap<ItemPath<'_>, ItemInfo>,
        external_crates: &HashMap<u32, rustdoc_types::ExternalCrate>,
    ) {
        let item_info = links_items_id.get(&item_path).expect("item not found");
        let parent_item_path = item_path.parent();
        let parent_item_info =
            parent_item_path.as_ref().and_then(|parent| links_items_id.get(&parent));

        // WIP! improve this mess!
        // 0 means local crate.
        if item_info.crate_id == 0 {
            let base_url =
                self.config.docs_rs_base_url.as_ref().map_or("https://docs.rs", String::as_str);
            let version = self.config.docs_rs_version.as_ref().map_or("latest", String::as_str);
            // WIP! do we need this?  can we extract it from somewhere?
            let crate_name = &self.crate_name;

            let mut url = match item_path.segments.len() {
                0 => unreachable!("an item should not have an empty path"),
                1 => format!("{base_url}/{crate_name}/{version}/"),
                _ => {
                    let drop_back =
                        LinkUrlResolver::is_nested_item_kind(item_info.kind.clone()) as usize + 1;
                    let url_path = item_path.segments.iter().dropping_back(drop_back).join("/");
                    format!("{base_url}/{crate_name}/{version}/{url_path}/")
                }
            };

            eprintln!("GREPME kind {:?}", item_info.kind);

            let last_component = LinkUrlResolver::url_last_component(
                item_info.kind.clone(),
                item_path.name(),
                parent_item_info.map(|i| i.kind.clone()),
                parent_item_path.as_ref().map(|i| i.name()),
            );

            url.push_str(&last_component);

            self.link_url.insert(link, url);
        } else {
            if let Some(rustdoc_types::ExternalCrate {
                // WIP! how about package name vs crate name
                html_root_url: Some(ref base_url),
                ..
            }) = external_crates.get(&item_info.crate_id)
            {
                // TODO Once we are able to use the stable version we can remove this.
                let base_url = base_url.replace("/nightly/", "/stable/");

                let mut url = match item_path.segments.len() {
                    0 => unreachable!("an item should not have an empty path"),
                    1 => format!("{base_url}"),
                    _ => {
                        let drop_back = LinkUrlResolver::is_nested_item_kind(item_info.kind.clone())
                            as usize
                            + 1;
                        let url_path = item_path.segments.iter().dropping_back(drop_back).join("/");
                        format!("{base_url}{url_path}/")
                    }
                };

                eprintln!("GREPME -> base_url {}", base_url);

                eprintln!("GREPME kind {:?}", item_info.kind);

                let last_component = LinkUrlResolver::url_last_component(
                    item_info.kind.clone(),
                    item_path.name(),
                    parent_item_info.map(|i| i.kind.clone()),
                    parent_item_path.as_ref().map(|i| i.name()),
                );

                url.push_str(&last_component);

                self.link_url.insert(link, url);
            }
        }
    }

    fn url(&self, link: &Link) -> Option<&str> {
        self.link_url.get(link).map(String::as_str)
    }

    fn is_intralink(link: &Link) -> bool {
        let has_lone_colon = || link.raw_link.replace("::", "").contains(':');

        !link.symbol().is_empty() && !link.raw_link.contains('/') && !has_lone_colon()
    }
}

fn rewrite_links(
    doc: &Doc,
    link_url_resolver: &LinkUrlResolver,
    emit_warning: &impl Fn(&str),
    config: &IntralinksConfig,
) -> Doc {
    let RewriteReferenceLinksResult { doc, reference_links_to_remove } =
        rewrite_reference_links_definitions(doc, link_url_resolver, emit_warning, config);

    let doc = rewrite_markdown_links(
        &doc,
        link_url_resolver,
        emit_warning,
        config,
        &reference_links_to_remove,
    );

    // TODO Refactor link removal code so that it all happens in a new phase and not inside the
    //      functions above.

    doc
}

enum MarkdownLinkAction {
    Link(Link),
    Preserve,
    Strip,
}

fn markdown_link(
    link: &Link,
    link_url_resolver: &LinkUrlResolver,
    emit_warning: &impl Fn(&str),
) -> MarkdownLinkAction {
    assert!(LinkUrlResolver::is_intralink(&link));

    match link_url_resolver.url(&link) {
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
    link_url_resolver: &LinkUrlResolver,
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
                    match LinkUrlResolver::is_intralink(&inline_link.link) {
                        true => match strip_links {
                            false => {
                                markdown_link(&inline_link.link, link_url_resolver, emit_warning)
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
    link_url_resolver: &LinkUrlResolver,
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
                    match LinkUrlResolver::is_intralink(&link_ref_def.link) {
                        true => match strip_links {
                            false => {
                                markdown_link(&link_ref_def.link, link_url_resolver, emit_warning)
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
            &HashMap::new(), // WIP!
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
            &HashMap::new(), // WIP!
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
            &HashMap::new(), // WIP!
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
            &HashMap::new(), // WIP!
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
            &HashMap::new(), // WIP!
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
            &HashMap::new(), // WIP!
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
