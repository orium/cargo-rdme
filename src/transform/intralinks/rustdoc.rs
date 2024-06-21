/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use crate::transform::intralinks::links::Link;
use crate::transform::intralinks::ItemPath;
use crate::transform::{IntralinkError, IntralinksConfig, IntralinksDocsRsConfig};
use crate::PackageTarget;
use itertools::Itertools;
use rustdoc_json::BuildError;
use rustdoc_types::{
    Crate, ExternalCrate, Id as ItemId, Impl, Item, ItemEnum, ItemSummary, MacroKind, Primitive,
    Struct, StructKind, Trait, Type,
};
use rustdoc_types::{Enum, ProcMacro, Union};
use std::collections::HashMap;
use std::path::{Path, PathBuf};

const EXPECTED_RUSTDOC_FORMAT_VERSION: u32 = 30;

fn crate_from_file(path: &Path) -> Result<Crate, IntralinkError> {
    let json = std::fs::read_to_string(path)
        .map_err(|io_error| IntralinkError::ReadRustdocError { io_error })?;
    serde_json::from_str(&json).map_err(|_| IntralinkError::ParseRustdocError)
}

fn crate_rustdoc_intralinks(c: &Crate) -> &HashMap<String, ItemId> {
    &c.index.get(&c.root).expect("root id not present in index").links
}

#[derive(Debug, Clone)]
struct ItemInfo<'a> {
    crate_id: u32,
    path: ItemPath<'a>,
    kind: ItemKind,
    parent_kind: Option<ItemKind>,
}

impl<'a> ItemInfo<'a> {
    fn new(
        crate_id: u32,
        path: ItemPath<'a>,
        kind: ItemKind,
        parent_kind: Option<ItemKind>,
    ) -> ItemInfo<'a> {
        ItemInfo { crate_id, path, kind, parent_kind }
    }

    fn from(
        item_summary: &'a ItemSummary,
        parent_kind: Option<ItemKind>,
        item_context: ItemContext,
    ) -> ItemInfo<'a> {
        ItemInfo::new(
            item_summary.crate_id,
            ItemPath::new(&item_summary.path),
            ItemKind::from_rustdoc_item_kind(&item_summary.kind, item_context),
            parent_kind,
        )
    }

    /// Merges all the information of both items.
    fn merge(&self, other: &ItemInfo<'a>) -> Option<ItemInfo<'a>> {
        if self.crate_id != other.crate_id {
            return None;
        }
        if self.path != other.path {
            return None;
        }
        if self.kind != other.kind {
            return None;
        }

        if self.parent_kind.zip(other.parent_kind).is_some_and(|(s, o)| s != o) {
            return None;
        }

        let merged = ItemInfo {
            crate_id: self.crate_id,
            path: self.path.clone(),
            kind: self.kind,
            parent_kind: self.parent_kind.or(other.parent_kind),
        };

        Some(merged)
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum ItemKind {
    Module,
    ExternCrate,
    Import,
    Struct,
    StructField,
    Union,
    Enum,
    Variant,
    Function,
    TypeAlias,
    OpaqueTy,
    Constant,
    Trait,
    TraitAlias,
    Impl,
    Static,
    ForeignType,
    Macro,
    ProcAttribute,
    ProcDerive,
    AssocConst,
    AssocType,
    Primitive,
    Keyword,

    // Kinds that do not exist in rustdoc_types::ItemKind:
    Method,
    TyMethod,
}

impl ItemKind {
    fn from_rustdoc_item_kind(
        kind: &rustdoc_types::ItemKind,
        item_context: ItemContext,
    ) -> ItemKind {
        match kind {
            rustdoc_types::ItemKind::Module => ItemKind::Module,
            rustdoc_types::ItemKind::ExternCrate => ItemKind::ExternCrate,
            rustdoc_types::ItemKind::Import => ItemKind::Import,
            rustdoc_types::ItemKind::Struct => ItemKind::Struct,
            rustdoc_types::ItemKind::StructField => ItemKind::StructField,
            rustdoc_types::ItemKind::Union => ItemKind::Union,
            rustdoc_types::ItemKind::Enum => ItemKind::Enum,
            rustdoc_types::ItemKind::Variant => ItemKind::Variant,
            rustdoc_types::ItemKind::Function => match item_context {
                ItemContext::Normal => ItemKind::Function,
                ItemContext::Impl => ItemKind::Method,
                ItemContext::Trait => ItemKind::TyMethod,
            },
            rustdoc_types::ItemKind::TypeAlias => ItemKind::TypeAlias,
            rustdoc_types::ItemKind::OpaqueTy => ItemKind::OpaqueTy,
            rustdoc_types::ItemKind::Constant => ItemKind::Constant,
            rustdoc_types::ItemKind::Trait => ItemKind::Trait,
            rustdoc_types::ItemKind::TraitAlias => ItemKind::TraitAlias,
            rustdoc_types::ItemKind::Impl => ItemKind::Impl,
            rustdoc_types::ItemKind::Static => ItemKind::Static,
            rustdoc_types::ItemKind::ForeignType => ItemKind::ForeignType,
            rustdoc_types::ItemKind::Macro => ItemKind::Macro,
            rustdoc_types::ItemKind::ProcAttribute => ItemKind::ProcAttribute,
            rustdoc_types::ItemKind::ProcDerive => ItemKind::ProcDerive,
            rustdoc_types::ItemKind::AssocConst => ItemKind::AssocConst,
            rustdoc_types::ItemKind::AssocType => ItemKind::AssocType,
            rustdoc_types::ItemKind::Primitive => ItemKind::Primitive,
            rustdoc_types::ItemKind::Keyword => ItemKind::Keyword,
        }
    }

    fn of_item(item: &Item, item_context: ItemContext) -> ItemKind {
        match item.inner {
            ItemEnum::Module(_) => ItemKind::Module,
            ItemEnum::ExternCrate { .. } => ItemKind::ExternCrate,
            ItemEnum::Import(_) => ItemKind::Import,
            ItemEnum::Union(_) => ItemKind::Union,
            ItemEnum::Struct(_) => ItemKind::Struct,
            ItemEnum::StructField(_) => ItemKind::StructField,
            ItemEnum::Enum(_) => ItemKind::Enum,
            ItemEnum::Variant(_) => ItemKind::Variant,
            ItemEnum::Function(_) => match item_context {
                ItemContext::Normal => ItemKind::Function,
                ItemContext::Impl => ItemKind::Method,
                ItemContext::Trait => ItemKind::TyMethod,
            },
            ItemEnum::Trait(_) => ItemKind::Trait,
            ItemEnum::TraitAlias(_) => ItemKind::TraitAlias,
            ItemEnum::Impl(_) => ItemKind::Impl,
            ItemEnum::TypeAlias(_) => ItemKind::TypeAlias,
            ItemEnum::OpaqueTy(_) => ItemKind::OpaqueTy,
            ItemEnum::Constant { .. } => ItemKind::Constant,
            ItemEnum::Static(_) => ItemKind::Static,
            ItemEnum::ForeignType => ItemKind::ForeignType,
            ItemEnum::Macro(_) => ItemKind::Macro,
            ItemEnum::ProcMacro(ProcMacro { kind: MacroKind::Bang, .. }) => ItemKind::Macro,
            ItemEnum::ProcMacro(ProcMacro { kind: MacroKind::Derive, .. }) => ItemKind::ProcDerive,
            ItemEnum::ProcMacro(ProcMacro { kind: MacroKind::Attr, .. }) => ItemKind::ProcAttribute,
            ItemEnum::Primitive(_) => ItemKind::Primitive,
            ItemEnum::AssocConst { .. } => ItemKind::AssocConst,
            ItemEnum::AssocType { .. } => ItemKind::AssocType,
        }
    }
}

fn child_item_ids<'a>(item: &'a Item) -> Box<dyn Iterator<Item = &'a ItemId> + 'a> {
    match &item.inner {
        ItemEnum::Struct(Struct { kind, impls, .. }) => {
            let fields_ids: Box<dyn Iterator<Item = &ItemId>> = match kind {
                StructKind::Unit => Box::new(std::iter::empty()),
                StructKind::Tuple(ids) => Box::new(ids.iter().filter_map(|id| id.as_ref())),
                StructKind::Plain { fields, .. } => Box::new(fields.iter()),
            };

            Box::new(fields_ids.chain(impls.iter()))
        }
        ItemEnum::Impl(Impl { trait_: Some(_), .. }) => Box::new(std::iter::empty()),
        ItemEnum::Impl(Impl { items: item_ids, for_, .. }) => match for_ {
            Type::ResolvedPath(_) => Box::new(item_ids.iter()),
            _ => Box::new(std::iter::empty()),
        },
        ItemEnum::Union(Union { fields, impls, .. }) => Box::new(fields.iter().chain(impls.iter())),
        ItemEnum::Enum(Enum { variants, impls, .. }) => {
            Box::new(variants.iter().chain(impls.iter()))
        }
        ItemEnum::Primitive(Primitive { impls, .. }) => Box::new(impls.iter()),
        ItemEnum::Trait(Trait { items, .. }) => {
            // We ignore the implementations of the trait as their items are not part of the trait
            // itself.
            Box::new(items.iter())
        }

        ItemEnum::Function(_)
        | ItemEnum::ExternCrate { .. }
        | ItemEnum::Import(_)
        | ItemEnum::Module(_)
        | ItemEnum::Constant { .. }
        | ItemEnum::Static(_)
        | ItemEnum::Macro(_)
        | ItemEnum::ProcMacro(_)
        | ItemEnum::AssocConst { .. }
        | ItemEnum::AssocType { .. }
        | ItemEnum::StructField(_)
        | ItemEnum::Variant(_)
        | ItemEnum::ForeignType
        | ItemEnum::TraitAlias(_)
        | ItemEnum::TypeAlias(_)
        | ItemEnum::OpaqueTy(_) => Box::new(std::iter::empty()),
    }
}

#[derive(Clone, Copy, Debug)]
enum ItemContext {
    Normal,
    Impl,
    Trait,
}

fn get_item_info<'a>(
    item_id: &ItemId,
    parent_path: &ItemPath<'a>,
    parent_kind: Option<ItemKind>,
    item_context: ItemContext,
    rustdoc_crate: &'a Crate,
) -> Option<ItemInfo<'a>> {
    match rustdoc_crate.paths.get(item_id) {
        Some(item_summary) => Some(ItemInfo::from(item_summary, parent_kind, item_context)),
        None => rustdoc_crate.index.get(item_id).map(|item| {
            let path = match item.name.as_ref() {
                None => parent_path.clone(),
                Some(name) => parent_path.add(name.clone()),
            };
            let item_kind = ItemKind::of_item(item, item_context);

            ItemInfo::new(item.crate_id, path, item_kind, parent_kind)
        }),
    }
}

fn transitive_items<'a>(
    item_id: &'a ItemId,
    item_info: &ItemInfo<'a>,
    item_context: ItemContext,
    rustdoc_crate: &'a Crate,
    items_info: &mut HashMap<&'a ItemId, ItemInfo<'a>>,
) {
    if item_info.kind != ItemKind::Impl {
        items_info
            .entry(item_id)
            .and_modify(|existing_item_info| {
                *existing_item_info =
                    existing_item_info.merge(item_info).expect("unmergeable item info");
            })
            .or_insert_with(|| item_info.clone());
    }

    let Some(item) = rustdoc_crate.index.get(item_id) else {
        // This item is not in the index for some reason...
        return;
    };

    let inner_item_context = match item.inner {
        ItemEnum::Trait(_) => ItemContext::Trait,
        ItemEnum::Impl(_) => ItemContext::Impl,
        _ => item_context,
    };

    for inner_item_id in child_item_ids(item) {
        // The inner_item_parent_kind is not just `item_info.kind` because we need to skip
        // kinds like `impl` blocks.
        let inner_item_parent_kind = match item.name {
            Some(_) => Some(item_info.kind),
            None => item_info.parent_kind,
        };

        let inner_item_info = get_item_info(
            inner_item_id,
            &item_info.path,
            inner_item_parent_kind,
            inner_item_context,
            rustdoc_crate,
        );

        if let Some(inner_item_info) = inner_item_info {
            transitive_items(
                inner_item_id,
                &inner_item_info,
                inner_item_context,
                rustdoc_crate,
                items_info,
            );
        }
    }
}

pub struct IntralinkResolver<'a> {
    link_url: HashMap<Link, String>,
    config: &'a IntralinksDocsRsConfig,
    package_name: &'a str,
}

impl<'a> IntralinkResolver<'a> {
    pub fn new(package_name: &'a str, config: &'a IntralinksDocsRsConfig) -> IntralinkResolver<'a> {
        IntralinkResolver { link_url: HashMap::new(), package_name, config }
    }

    fn url_segment(kind: ItemKind, name: &str) -> String {
        match kind {
            ItemKind::Module => format!("{name}/"),
            ItemKind::Struct => format!("struct.{name}.html"),
            ItemKind::StructField => format!("#structfield.{name}"),
            ItemKind::Union => format!("union.{name}.html"),
            ItemKind::Enum => format!("enum.{name}.html"),
            ItemKind::Variant => format!("#variant.{name}"),
            ItemKind::Function => format!("fn.{name}.html"),
            ItemKind::Method => format!("#method.{name}"),
            ItemKind::TyMethod => format!("#tymethod.{name}"),
            ItemKind::TypeAlias => format!("type.{name}.html"),
            ItemKind::OpaqueTy => format!("type.{name}.html"),
            ItemKind::Constant => format!("const.{name}.html"),
            ItemKind::Trait => format!("trait.{name}.html"),
            ItemKind::TraitAlias => format!("traitalias.{name}.html"),
            ItemKind::Static => format!("static.{name}.html"),
            ItemKind::Macro => format!("macro.{name}.html"),
            ItemKind::ProcAttribute => format!("attr.{name}.html"),
            ItemKind::ProcDerive => format!("derive.{name}.html"),
            ItemKind::AssocConst => {
                format!("#associatedconstant.{name}")
            }
            ItemKind::AssocType => format!("#associatedtype.{name}"),
            ItemKind::Primitive => format!("primitive.{name}.html"),

            ItemKind::Keyword
            | ItemKind::ExternCrate
            | ItemKind::Import
            | ItemKind::Impl
            | ItemKind::ForeignType => {
                unreachable!("items of kind {:?} cannot be intralinked to", kind);
            }
        }
    }

    fn is_stdlib_crate(external_crate: &ExternalCrate) -> bool {
        external_crate
            .html_root_url
            .as_deref()
            .is_some_and(|base_url| base_url.starts_with("https://doc.rust-lang.org/"))
    }

    fn make_url(base_url: &str, package_name: &str, version: &str, url_path: &str) -> String {
        format!("{base_url}/{package_name}/{version}/{url_path}")
    }

    fn add(
        &mut self,
        link: Link,
        item_info: &ItemInfo,
        external_crates: &HashMap<u32, ExternalCrate>,
    ) {
        let docs_rs_base_url = self.config.docs_rs_base_url.as_deref().unwrap_or("https://docs.rs");

        let path_segment_kind = |i: usize| match item_info.path.len() - i {
            1 => item_info.kind,
            2 => item_info.parent_kind.unwrap_or(ItemKind::Module),
            _ => ItemKind::Module,
        };
        let url_path = item_info
            .path
            .segments()
            .enumerate()
            .map(|(i, segment)| (segment, path_segment_kind(i)))
            .map(|(segment, item_kind)| IntralinkResolver::url_segment(item_kind, segment))
            .join("");

        let url = match item_info.crate_id {
            // Local crate has id 0.
            0 => {
                let version = self.config.docs_rs_version.as_deref().unwrap_or("latest");
                let package_name = &self.package_name;

                Self::make_url(docs_rs_base_url, package_name, version, &url_path)
            }
            // External crate
            _ => {
                let Some(external_crate) = external_crates.get(&item_info.crate_id) else {
                    return;
                };

                match external_crate.html_root_url.as_deref() {
                    Some(base_url) => {
                        let base_url = match Self::is_stdlib_crate(external_crate) {
                            true => {
                                // TODO Once we are able to use the stable version we can remove this
                                //      (https://github.com/rust-lang/rust/issues/76578).
                                base_url
                                    .strip_suffix("/nightly/")
                                    .map_or_else(|| base_url.to_owned(), |p| format!("{p}/stable/"))
                            }
                            false => base_url.to_owned(),
                        };

                        format!("{base_url}{url_path}")
                    }
                    None => {
                        let crate_name = &external_crate.name;

                        // TODO We are using the crate name instead of the package name: that means that
                        //      we might generate a wrong url. In most cases the crate name matches the
                        //      package name. When it doesn't it is often because underscores in the
                        //      crate name becomes dashes in the package name. Fortunately `docs.rs`
                        //      will redirect in that case (e.g. https://docs.rs/tower_service/ will
                        //      redirect to https://docs.rs/tower-service/latest/tower_service/).
                        // TODO We shouldn't hardcode "latest" here: we should get that information from
                        //      the version rustdoc determined the crate was using.
                        Self::make_url(docs_rs_base_url, crate_name, "latest", &url_path)
                    }
                }
            }
        };

        self.link_url.insert(link, url);
    }

    pub fn resolve_link(&self, link: &Link) -> Option<&str> {
        self.link_url.get(link).map(String::as_str)
    }

    pub fn is_intralink(link: &Link) -> bool {
        let has_lone_colon = || link.raw_link.replace("::", "").contains(':');

        !link.symbol().is_empty() && !link.raw_link.contains('/') && !has_lone_colon()
    }
}

fn run_rustdoc(
    package_target: &PackageTarget,
    workspace_package: Option<&str>,
    manifest_path: &PathBuf,
) -> Result<Crate, IntralinkError> {
    // WIP! you can check if rustup is installed with rustup_installed.

    let rustdoc_json_path: PathBuf = {
        let target: rustdoc_json::PackageTarget = match package_target {
            PackageTarget::Bin { crate_name } => {
                rustdoc_json::PackageTarget::Bin(crate_name.clone())
            }
            PackageTarget::Lib => rustdoc_json::PackageTarget::Lib,
        };
        let mut stderr = Vec::new();

        let mut builder = rustdoc_json::Builder::default()
            // TODO Use stable when this stabilizes (https://github.com/rust-lang/rust/issues/76578).
            .toolchain("nightly")
            .manifest_path(manifest_path)
            .document_private_items(true)
            // WIP!simple We need to parameterize the features based on the configuration
            //      also "no default features"
            .all_features(true)
            .quiet(true)
            .color(rustdoc_json::Color::Never)
            .package_target(target);

        if let Some(package) = workspace_package {
            builder = builder.package(package);
        }

        let result = builder.build_with_captured_output(std::io::sink(), &mut stderr);

        result.map_err(|error| match error {
            BuildError::BuildRustdocJsonError => match stderr.is_empty() {
                true => IntralinkError::BuildRustdocError {
                    stderr: "Weirdly, rustdoc did not write anything to stderr".to_owned(),
                },
                false => IntralinkError::BuildRustdocError {
                    stderr: String::from_utf8_lossy(&stderr).into_owned(),
                },
            },
            e => IntralinkError::RustdocError { error: e },
        })?
    };

    let rustdoc_crate = crate_from_file(&rustdoc_json_path)?;

    match rustdoc_crate.format_version {
        EXPECTED_RUSTDOC_FORMAT_VERSION => Ok(rustdoc_crate),
        format_version => Err(IntralinkError::UnsupportedRustdocFormatVersion {
            version: format_version,
            expected_version: EXPECTED_RUSTDOC_FORMAT_VERSION,
        }),
    }
}

fn items_info(rustdoc_crate: &Crate) -> HashMap<&ItemId, ItemInfo<'_>> {
    let mut items_info: HashMap<&ItemId, ItemInfo<'_>> =
        HashMap::with_capacity(rustdoc_crate.index.len());

    for (item_id, item_summary) in &rustdoc_crate.paths {
        let item_info = ItemInfo::from(item_summary, None, ItemContext::Normal);

        transitive_items(item_id, &item_info, ItemContext::Normal, rustdoc_crate, &mut items_info);
    }

    items_info
}

pub fn create_intralink_resolver<'a>(
    package_name: &'a str,
    package_target: &PackageTarget,
    workspace_package: Option<&str>,
    manifest_path: &PathBuf,
    config: &'a IntralinksConfig,
) -> Result<IntralinkResolver<'a>, IntralinkError> {
    let rustdoc_crate = run_rustdoc(package_target, workspace_package, manifest_path)?;

    let items_info: HashMap<&ItemId, ItemInfo<'_>> = items_info(&rustdoc_crate);
    let links_items_id = crate_rustdoc_intralinks(&rustdoc_crate);
    let mut intralink_resolver = IntralinkResolver::new(package_name, &config.docs_rs);

    for (link, item_id) in links_items_id {
        let link = Link::new(link.clone());
        let Some(item_info) = items_info.get(item_id) else {
            // We will fail when we try to create the link and will emit a warning there.
            continue;
        };

        intralink_resolver.add(link, item_info, &rustdoc_crate.external_crates);
    }

    Ok(intralink_resolver)
}

// WIP! move the tests that belong here
