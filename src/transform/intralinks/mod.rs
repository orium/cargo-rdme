/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use crate::markdown::Markdown;
use crate::transform::DocTransform;
use crate::utils::MarkdownItemIterator;
use crate::Doc;
use module_walker::walk_module_file;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use syn::{Item, ItemMod};
use thiserror::Error;

mod module_walker;

#[derive(Error, Debug)]
pub enum IntralinkError {
    #[error("IO error: {0}")]
    IOError(std::io::Error),
    #[error("failed to analyzing code: {0}")]
    AstWalkError(module_walker::ModuleWalkError),
    #[error("failed to load standard library: {0}")]
    LoadStdLibError(String),
}

impl From<std::io::Error> for IntralinkError {
    fn from(err: std::io::Error) -> Self {
        IntralinkError::IOError(err)
    }
}

impl From<module_walker::ModuleWalkError> for IntralinkError {
    fn from(err: module_walker::ModuleWalkError) -> Self {
        IntralinkError::AstWalkError(err)
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
    entrypoint: PathBuf,
    emit_warning: F,
    config: IntralinksConfig,
}

impl<F> DocTransformIntralinks<F>
where
    F: Fn(&str),
{
    pub fn new(
        crate_name: impl Into<String>,
        entrypoint: impl AsRef<Path>,
        emit_warning: F,
        config: Option<IntralinksConfig>,
    ) -> DocTransformIntralinks<F> {
        DocTransformIntralinks {
            crate_name: crate_name.into(),
            entrypoint: entrypoint.as_ref().to_path_buf(),
            emit_warning,
            config: config.unwrap_or_default(),
        }
    }
}

impl<F> DocTransform for DocTransformIntralinks<F>
where
    F: Fn(&str),
{
    type E = IntralinkError;

    fn transform(&self, doc: &Doc) -> Result<Doc, IntralinkError> {
        let symbols: HashSet<ItemPath> = extract_markdown_intralink_symbols(doc);

        // If there are no intralinks in the doc don't even bother doing anything else.
        if symbols.is_empty() {
            return Ok(doc.clone());
        }

        // We only load symbols type information when we need them.
        let symbols_type = match self.config.strip_links.unwrap_or(false) {
            false => load_symbols_type(&self.entrypoint, &symbols, &self.emit_warning)?,
            true => HashMap::new(),
        };

        let doc_with_new_links = rewrite_markdown_links(
            doc,
            &symbols_type,
            &self.crate_name,
            &self.emit_warning,
            &self.config,
        );

        let doc_with_new_links_and_ref_defs = rewrite_reference_definitions(
            &doc_with_new_links,
            &symbols_type,
            &self.crate_name,
            &self.emit_warning,
            &self.config,
        );

        Ok(doc_with_new_links_and_ref_defs)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub(self) enum ItemPathAnchor {
    Root,
    Crate,
}

/// The rust path of an item, such as `foo::bar::is_prime` or `crate`.
#[derive(Clone)]
pub(self) struct ItemPath {
    pub anchor: ItemPathAnchor,

    /// This path vector can be shared and can end after the item_path we are representing.
    /// This allow us to have a faster implementation for `ItemPath::all_ancestors()`.
    path_shared: Rc<Vec<String>>,
    path_end: usize,
}

impl ItemPath {
    fn new(anchor: ItemPathAnchor) -> ItemPath {
        ItemPath { anchor, path_shared: Rc::new(Vec::new()), path_end: 0 }
    }

    fn root(crate_name: &str) -> ItemPath {
        ItemPath::new(ItemPathAnchor::Root).join(crate_name)
    }

    fn from_string(s: &str) -> Option<ItemPath> {
        let anchor;
        let rest;

        if let Some(r) = s.strip_prefix("::") {
            anchor = ItemPathAnchor::Root;
            rest = r;
        } else if s == "crate" {
            return Some(ItemPath::new(ItemPathAnchor::Crate));
        } else if let Some(r) = s.strip_prefix("crate::") {
            anchor = ItemPathAnchor::Crate;
            rest = r;
        } else {
            return None;
        }

        if rest.is_empty() {
            return None;
        }

        let path: Rc<Vec<String>> = Rc::new(rest.split("::").map(str::to_owned).collect());

        Some(ItemPath { anchor, path_end: path.len(), path_shared: path })
    }

    fn path(&self) -> &[String] {
        &self.path_shared[0..self.path_end]
    }

    fn is_toplevel(&self) -> bool {
        match self.anchor {
            ItemPathAnchor::Root => self.path_end <= 1,
            ItemPathAnchor::Crate => self.path_end == 0,
        }
    }

    fn parent(mut self) -> Option<ItemPath> {
        match self.is_toplevel() {
            true => None,
            false => {
                self.path_end -= 1;
                Some(self)
            }
        }
    }

    fn join(mut self, s: &str) -> ItemPath {
        let path = Rc::make_mut(&mut self.path_shared);
        path.truncate(self.path_end);
        path.push(s.to_string());
        self.path_end += 1;
        self
    }

    fn all_ancestors(&self) -> impl Iterator<Item = ItemPath> {
        let first_ancestor = self.clone().parent();

        std::iter::successors(first_ancestor, |ancestor| ancestor.clone().parent())
    }
}

impl PartialEq for ItemPath {
    fn eq(&self, other: &Self) -> bool {
        self.anchor == other.anchor && self.path() == other.path()
    }
}

impl Eq for ItemPath {}

impl Hash for ItemPath {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.anchor.hash(state);
        self.path().hash(state);
    }
}

impl fmt::Display for ItemPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self.anchor {
            ItemPathAnchor::Root => (),
            ItemPathAnchor::Crate => f.write_str("crate")?,
        }

        for s in self.path().iter() {
            f.write_str("::")?;
            f.write_str(s)?;
        }

        Ok(())
    }
}

impl fmt::Debug for ItemPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        fmt::Display::fmt(self, f)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SymbolType {
    Crate,
    Struct,
    Trait,
    Enum,
    Union,
    Type,
    Mod,
    Macro,
    Const,
    Fn,
    Static,
}

fn symbol_type(module: &ItemPath, item: &Item) -> Option<(ItemPath, SymbolType)> {
    let (ident, symbol_type) = match item {
        Item::Enum(e) => (e.ident.to_string(), SymbolType::Enum),
        Item::Struct(s) => (s.ident.to_string(), SymbolType::Struct),
        Item::Trait(t) => (t.ident.to_string(), SymbolType::Trait),
        Item::Union(u) => (u.ident.to_string(), SymbolType::Union),
        Item::Type(t) => (t.ident.to_string(), SymbolType::Type),
        Item::Mod(m) => (m.ident.to_string(), SymbolType::Mod),
        Item::Macro(syn::ItemMacro { ident: Some(ident), .. }) => {
            (ident.to_string(), SymbolType::Macro)
        }
        Item::Macro2(m) => (m.ident.to_string(), SymbolType::Macro),
        Item::Const(c) => (c.ident.to_string(), SymbolType::Const),
        Item::Fn(f) => (f.sig.ident.to_string(), SymbolType::Fn),
        Item::Static(s) => (s.ident.to_string(), SymbolType::Static),

        _ => return None,
    };

    Some((module.clone().join(&ident), symbol_type))
}

fn is_cfg_test(attribute: &syn::Attribute) -> bool {
    let test_attribute: syn::Attribute = syn::parse_quote!(#[cfg(test)]);

    *attribute == test_attribute
}

fn visit_module_item(
    save_symbol: impl Fn(&ItemPath) -> bool,
    symbols_type: &mut HashMap<ItemPath, SymbolType>,
    module: &ItemPath,
    item: &Item,
) {
    if let Some((symbol, symbol_type)) = symbol_type(module, item) {
        if save_symbol(&symbol) {
            symbols_type.insert(symbol, symbol_type);
        }
    }
}

/// Returns whether we should explore a module.
fn check_explore_module(
    should_explore_module: impl Fn(&ItemPath) -> bool,
    modules_visited: &mut HashSet<ItemPath>,
    mod_symbol: &ItemPath,
    mod_item: &ItemMod,
) -> bool {
    // Conditional compilation can create multiple module definitions, e.g.
    //
    // ```
    // #[cfg(foo)]
    // mod a {}
    // #[cfg(not(foo))]
    // mod a {}
    // ```
    //
    // We choose to consider the first one only.
    if modules_visited.contains(mod_symbol) {
        return false;
    }

    // If a module is gated by `#[cfg(test)]` we skip it.  This happens sometimes in the
    // standard library, and we want to explore the correct, non-test, module.
    if mod_item.attrs.iter().any(is_cfg_test) {
        return false;
    }

    let explore = should_explore_module(mod_symbol);

    if explore {
        modules_visited.insert(mod_symbol.clone());
    }

    explore
}

fn explore_crate<P: AsRef<Path>>(
    file: P,
    crate_symbol: &ItemPath,
    symbols: &HashSet<ItemPath>,
    modules_to_explore: &HashSet<ItemPath>,
    symbols_type: &mut HashMap<ItemPath, SymbolType>,
    emit_warning: &impl Fn(&str),
) -> Result<(), module_walker::ModuleWalkError> {
    let mut modules_visited: HashSet<ItemPath> = HashSet::new();

    // Walking the module only visits items, which means we need to add the root `crate` explicitly.
    symbols_type.insert(crate_symbol.clone(), SymbolType::Crate);

    let mut visit = |module: &ItemPath, item: &Item| {
        visit_module_item(|symbol| symbols.contains(symbol), symbols_type, module, item);
    };

    let mut explore_module = |mod_symbol: &ItemPath, mod_item: &ItemMod| -> bool {
        check_explore_module(
            |mod_symbol| modules_to_explore.contains(mod_symbol),
            &mut modules_visited,
            mod_symbol,
            mod_item,
        )
    };

    walk_module_file(file, crate_symbol, &mut visit, &mut explore_module, emit_warning)
}

fn load_symbols_type<P: AsRef<Path>>(
    entry_point: P,
    symbols: &HashSet<ItemPath>,
    emit_warning: &impl Fn(&str),
) -> Result<HashMap<ItemPath, SymbolType>, IntralinkError> {
    let modules_to_explore: HashSet<ItemPath> = all_supermodules(symbols.iter());
    let mut symbols_type: HashMap<ItemPath, SymbolType> = HashMap::new();

    // Only load standard library information if needed.
    let std_lib_crates = match references_standard_library(symbols) {
        true => get_standard_libraries()?,
        false => Vec::new(),
    };

    for Crate { name, entrypoint } in std_lib_crates {
        explore_crate(
            entrypoint,
            &ItemPath::root(&name),
            symbols,
            &modules_to_explore,
            &mut symbols_type,
            emit_warning,
        )?;
    }

    explore_crate(
        entry_point,
        &ItemPath::new(ItemPathAnchor::Crate),
        symbols,
        &modules_to_explore,
        &mut symbols_type,
        emit_warning,
    )?;

    Ok(symbols_type)
}

/// Create a set with all supermodules in `symbols`.  For instance, if `symbols` is
/// `{crate::foo::bar::baz, crate::baz::mumble}` it will return
/// `{crate, crate::foo, crate::foo::bar, crate::baz}`.
fn all_supermodules<'a>(symbols: impl Iterator<Item = &'a ItemPath>) -> HashSet<ItemPath> {
    symbols.into_iter().flat_map(ItemPath::all_ancestors).collect()
}

#[derive(Eq, PartialEq, Clone, Debug)]
struct MarkdownInlineLink {
    text: String,
    inner: MarkdownLink,
}

#[derive(Eq, PartialEq, Clone, Debug)]
struct MarkdownLink {
    raw_link: String,
}

impl From<String> for MarkdownLink {
    fn from(raw_link: String) -> Self {
        Self { raw_link }
    }
}

impl std::fmt::Display for MarkdownLink {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.raw_link.fmt(f)
    }
}

impl MarkdownLink {
    fn link_as_item_path(&self) -> Option<ItemPath> {
        let link = self.split_link_fragment().0;

        ItemPath::from_string(link)
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
                    strip_last_backtick(strip_backtick_end, f),
                )
            }
        }
    }

    fn link_fragment(&self) -> &str {
        self.split_link_fragment().1
    }
}

fn markdown_inline_link_iterator(markdown: &Markdown) -> MarkdownItemIterator<MarkdownInlineLink> {
    use pulldown_cmark::{Event, LinkType, Options, Parser, Tag};

    let source = markdown.as_string();
    let parser = Parser::new_ext(source, Options::all());
    let mut in_link = false;
    let mut start_text = 0;
    let mut end_text = 0;

    let iter = parser.into_offset_iter().filter_map(move |(event, range)| match event {
        Event::Start(Tag::Link(LinkType::Inline, ..)) => {
            in_link = true;
            start_text = range.start + 1;
            end_text = range.end;
            None
        }
        Event::End(Tag::Link(LinkType::Inline, ..)) => {
            in_link = false;

            let text = source[start_text..end_text].to_owned();
            let link = source[(end_text + 2)..(range.end - 1)].to_owned().into();

            Some((range.into(), MarkdownInlineLink { text, inner: link }))
        }
        _ => {
            if in_link {
                end_text = range.end;
            }
            None
        }
    });

    MarkdownItemIterator::new(source, iter)
}

fn extract_markdown_intralink_symbols(doc: &Doc) -> HashSet<ItemPath> {
    markdown_inline_link_iterator(&doc.markdown)
        .items()
        .flat_map(|l| l.inner.link_as_item_path().into_iter())
        .collect()
}

fn documentation_url(
    symbol: &ItemPath,
    typ: SymbolType,
    crate_name: &str,
    config: &IntralinksDocsRsConfig,
) -> String {
    let package_name = crate_name.replace('-', "_");
    let mut path = symbol.path();

    let mut link = match symbol.anchor {
        ItemPathAnchor::Root => {
            let std_crate_name = &path[0];
            path = &path[1..];
            format!("https://doc.rust-lang.org/stable/{}/", std_crate_name)
        }
        ItemPathAnchor::Crate => {
            let base_url =
                config.docs_rs_base_url.as_ref().map_or("https://docs.rs", String::as_str);
            let version = config.docs_rs_version.as_ref().map_or("latest", String::as_str);

            format!("{}/{}/{}/{}/", base_url, crate_name, version, package_name)
        }
    };

    if SymbolType::Crate == typ {
        return link;
    }

    for s in path.iter().rev().skip(1).rev() {
        link.push_str(s);
        link.push('/');
    }

    let name = path.last().unwrap_or_else(|| panic!("failed to get last component of {}", symbol));

    match typ {
        SymbolType::Crate => unreachable!(),
        SymbolType::Struct => link.push_str(&format!("struct.{}.html", name)),
        SymbolType::Trait => link.push_str(&format!("trait.{}.html", name)),
        SymbolType::Enum => link.push_str(&format!("enum.{}.html", name)),
        SymbolType::Union => link.push_str(&format!("union.{}.html", name)),
        SymbolType::Type => link.push_str(&format!("type.{}.html", name)),
        SymbolType::Mod => link.push_str(&format!("{}/", name)),
        SymbolType::Macro => link.push_str(&format!("macro.{}.html", name)),
        SymbolType::Const => link.push_str(&format!("const.{}.html", name)),
        SymbolType::Fn => link.push_str(&format!("fn.{}.html", name)),
        SymbolType::Static => link.push_str(&format!("static.{}.html", name)),
    }

    link
}

enum MarkdownLinkAction {
    Link(String),
    Preserve,
    Strip,
}

fn markdown_link(
    link: &MarkdownLink,
    symbols_type: &HashMap<ItemPath, SymbolType>,
    crate_name: &str,
    emit_warning: &impl Fn(&str),
    config: &IntralinksConfig,
) -> MarkdownLinkAction {
    match link.link_as_item_path() {
        Some(symbol) if symbols_type.contains_key(&symbol) => {
            let typ = symbols_type[&symbol];
            let new_link = documentation_url(&symbol, typ, crate_name, &config.docs_rs);

            MarkdownLinkAction::Link(format!("{}{}", new_link, link.link_fragment()))
        }
        Some(symbol) => {
            emit_warning(&format!("Could not find definition of `{}`.", symbol));

            // This was an intralink, but we were not able to generate a link.
            MarkdownLinkAction::Strip
        }
        _ => MarkdownLinkAction::Preserve,
    }
}

fn rewrite_markdown_links(
    doc: &Doc,
    symbols_type: &HashMap<ItemPath, SymbolType>,
    crate_name: &str,
    emit_warning: &impl Fn(&str),
    config: &IntralinksConfig,
) -> Doc {
    use crate::utils::ItemOrOther;

    let mut new_doc = String::with_capacity(doc.as_string().len());

    for item_or_other in markdown_inline_link_iterator(&doc.markdown).complete() {
        match item_or_other {
            ItemOrOther::Item(link) => {
                let strip_links = config.strip_links.unwrap_or(false);

                let markdown_link: MarkdownLinkAction = match strip_links {
                    false => {
                        markdown_link(&link.inner, symbols_type, crate_name, emit_warning, config)
                    }
                    true => match link.inner.link_as_item_path() {
                        None => MarkdownLinkAction::Preserve,
                        Some(_) => MarkdownLinkAction::Strip,
                    },
                };

                match markdown_link {
                    MarkdownLinkAction::Link(markdown_link) => {
                        new_doc.push_str(&format!("[{}]({})", link.text, markdown_link));
                    }
                    MarkdownLinkAction::Preserve => {
                        new_doc.push_str(&format!("[{}]({})", link.text, link.inner));
                    }
                    MarkdownLinkAction::Strip => {
                        new_doc.push_str(&link.text);
                    }
                }
            }
            ItemOrOther::Other(other) => {
                new_doc.push_str(other);
            }
        }
    }

    Doc::from_str(new_doc)
}

fn rewrite_reference_definitions(
    doc: &Doc,
    symbols_type: &HashMap<ItemPath, SymbolType>,
    crate_name: &str,
    emit_warning: &impl Fn(&str),
    config: &IntralinksConfig,
) -> Doc {
    use crate::utils::ItemOrOther;
    use pulldown_cmark::{LinkDef, Options, Parser};

    let source = doc.as_string();
    let parser = Rc::new(Parser::new_ext(source, Options::all()));

    // It seems `Parser::reference_definitions` need not traverse the definitions in order. But
    // `MarkdownItemIterator::complete` expects the iterated-over spans to be in order.
    let btree = parser
        .reference_definitions()
        .iter()
        .map(|ref_def| (crate::utils::Span::from(ref_def.1.span.clone()), ref_def))
        .collect::<std::collections::BTreeMap<_, _>>();

    let iter = MarkdownItemIterator::new(source, btree.into_iter());

    let mut new_doc = String::with_capacity(doc.as_string().len());

    for item_or_other in iter.complete() {
        match item_or_other {
            ItemOrOther::Item((key, LinkDef { dest, title, span })) => {
                let link = MarkdownLink { raw_link: dest.to_string() };
                let markdown_link =
                    markdown_link(&link, symbols_type, crate_name, emit_warning, config);

                match (&markdown_link, title) {
                    (MarkdownLinkAction::Link(markdown_link), None) => {
                        new_doc.push_str(&format!("[{key}]: {markdown_link}"));
                    }
                    _ => {
                        if matches!(markdown_link, MarkdownLinkAction::Link(_)) {
                            emit_warning(
                                "Rewriting link definitions with titles is not supported.",
                            );
                        }
                        new_doc.push_str(&source[span.clone()]);
                    }
                }
            }
            ItemOrOther::Other(other) => {
                new_doc.push_str(other);
            }
        }
    }

    Doc::from_str(new_doc)
}

fn get_rustc_sysroot_libraries_dir() -> Result<PathBuf, IntralinkError> {
    use std::process::Command;

    let output = Command::new("rustc")
        .args(["--print=sysroot"])
        .output()
        .map_err(|e| IntralinkError::LoadStdLibError(format!("failed to run rustc: {}", e)))?;

    let s = String::from_utf8(output.stdout).expect("unexpected output from rustc");
    let sysroot = PathBuf::from(s.trim());
    let src_path = sysroot.join("lib").join("rustlib").join("src").join("rust").join("library");

    match src_path.is_dir() {
        false => Err(IntralinkError::LoadStdLibError(format!(
            "Cannot find rust standard library in \"{}\"",
            src_path.display()
        ))),
        true => Ok(src_path),
    }
}

#[derive(Debug)]
struct Crate {
    name: String,
    entrypoint: PathBuf,
}

fn references_standard_library(symbols: &HashSet<ItemPath>) -> bool {
    // The only way to reference standard libraries that we support is with a intra-link of form `::⋯`.
    symbols.iter().any(|symbol| symbol.anchor == ItemPathAnchor::Root)
}

fn get_standard_libraries() -> Result<Vec<Crate>, IntralinkError> {
    let libraries_dir = get_rustc_sysroot_libraries_dir()?;
    let mut std_libs = Vec::with_capacity(64);

    for entry in std::fs::read_dir(libraries_dir)? {
        let entry = entry?;
        let project_dir_path = entry.path();
        let cargo_manifest_path = project_dir_path.join("Cargo.toml");
        let lib_entrypoint = project_dir_path.join("src").join("lib.rs");

        if cargo_manifest_path.is_file() && lib_entrypoint.is_file() {
            let crate_name =
                crate::project_package_name(&cargo_manifest_path).ok_or_else(|| {
                    IntralinkError::LoadStdLibError(format!(
                        "failed to load manifest in \"{}\"",
                        cargo_manifest_path.display()
                    ))
                })?;
            let crate_info = Crate { name: crate_name, entrypoint: lib_entrypoint };

            std_libs.push(crate_info);
        }
    }

    Ok(std_libs)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::Span;
    use indoc::indoc;
    use module_walker::walk_module_items;
    use std::cell::RefCell;

    fn item_path(id: &str) -> ItemPath {
        ItemPath::from_string(id).unwrap()
    }

    #[test]
    fn test_item_path_is_toplevel() {
        assert!(!item_path("crate::baz::mumble").is_toplevel());
        assert!(!item_path("::std::baz::mumble").is_toplevel());
        assert!(!item_path("crate::baz").is_toplevel());
        assert!(!item_path("::std::baz").is_toplevel());
        assert!(item_path("crate").is_toplevel());
        assert!(item_path("::std").is_toplevel());
    }

    #[test]
    fn test_item_path_parent() {
        assert_eq!(item_path("crate::baz::mumble").parent(), Some(item_path("crate::baz")));
        assert_eq!(item_path("::std::baz::mumble").parent(), Some(item_path("::std::baz")));
        assert_eq!(item_path("crate::baz").parent(), Some(item_path("crate")));
        assert_eq!(item_path("::std::baz").parent(), Some(item_path("::std")));
        assert_eq!(item_path("crate").parent(), None);
        assert_eq!(item_path("::std").parent(), None);
    }

    #[test]
    fn test_item_path_join() {
        assert_eq!(item_path("crate::foo").join("bar"), item_path("crate::foo::bar"),);
        assert_eq!(item_path("::std::foo").join("bar"), item_path("::std::foo::bar"),);

        assert_eq!(
            item_path("::std::foo::bar").parent().unwrap().join("baz"),
            item_path("::std::foo::baz"),
        );
    }

    #[test]
    fn test_markdown_link_iterator() {
        let markdown =
            Markdown::from_str("A [some text] [another](http://foo.com), [another][one]");

        let mut iter = markdown_inline_link_iterator(&markdown).items_with_spans();
        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownInlineLink {
                text: "another".to_owned(),
                inner: "http://foo.com".to_owned().into()
            }
        );
        assert_eq!(&markdown.as_string()[start..end], "[another](http://foo.com)");

        assert_eq!(iter.next(), None);

        let markdown = Markdown::from_str("[another](http://foo.com)[another][one]");
        let mut iter = markdown_inline_link_iterator(&markdown).items_with_spans();

        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownInlineLink {
                text: "another".to_owned(),
                inner: "http://foo.com".to_owned().into()
            }
        );
        assert_eq!(&markdown.as_string()[start..end], "[another](http://foo.com)");

        assert_eq!(iter.next(), None);

        let markdown = Markdown::from_str("A [some [text]], [another [text] (foo)](http://foo.com/foo(bar)), [another [] one][foo[]bar]");
        let mut iter = markdown_inline_link_iterator(&markdown).items_with_spans();

        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownInlineLink {
                text: "another [text] (foo)".to_owned(),
                inner: "http://foo.com/foo(bar)".to_owned().into(),
            }
        );
        assert_eq!(
            &markdown.as_string()[start..end],
            "[another [text] (foo)](http://foo.com/foo(bar))"
        );

        assert_eq!(iter.next(), None);

        let markdown = Markdown::from_str(
            "A [some \\]text], [another](http://foo.\\(com\\)), [another\\]][one\\]]",
        );
        let mut iter = markdown_inline_link_iterator(&markdown).items_with_spans();

        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownInlineLink {
                text: "another".to_owned(),
                inner: r"http://foo.\(com\)".to_owned().into(),
            }
        );
        assert_eq!(&markdown.as_string()[start..end], r"[another](http://foo.\(com\))");

        assert_eq!(iter.next(), None);

        let markdown = Markdown::from_str("A `this is no link [link](http://foo.com)`");
        let mut iter = markdown_inline_link_iterator(&markdown).items_with_spans();

        assert_eq!(iter.next(), None);

        let markdown = Markdown::from_str("A\n```\nthis is no link [link](http://foo.com)\n```");
        let mut iter = markdown_inline_link_iterator(&markdown).items_with_spans();

        assert_eq!(iter.next(), None);

        let markdown = Markdown::from_str("A [link with `code`!](http://foo.com)!");
        let mut iter = markdown_inline_link_iterator(&markdown).items_with_spans();

        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownInlineLink {
                text: "link with `code`!".to_owned(),
                inner: "http://foo.com".to_owned().into(),
            }
        );
        assert_eq!(&markdown.as_string()[start..end], "[link with `code`!](http://foo.com)");

        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_all_supermodules() {
        let symbols = [
            item_path("crate::foo::bar::baz"),
            item_path("crate::baz::mumble"),
            item_path("::std::vec::Vec"),
        ];
        let expected: HashSet<ItemPath> = [
            item_path("crate"),
            item_path("crate::foo"),
            item_path("crate::foo::bar"),
            item_path("crate::baz"),
            item_path("::std"),
            item_path("::std::vec"),
        ]
        .into_iter()
        .collect();

        assert_eq!(all_supermodules(symbols.iter()), expected);
    }

    fn explore_crate(
        ast: &[Item],
        dir: &Path,
        crate_symbol: &ItemPath,
        should_explore_module: impl Fn(&ItemPath) -> bool,
        symbols_type: &mut HashMap<ItemPath, SymbolType>,
        emit_warning: impl Fn(&str),
    ) {
        let mut modules_visited: HashSet<ItemPath> = HashSet::new();

        symbols_type.insert(crate_symbol.clone(), SymbolType::Crate);

        let mut visit = |module: &ItemPath, item: &Item| {
            visit_module_item(|_| true, symbols_type, module, item);
        };

        let mut explore_module = |mod_symbol: &ItemPath, mod_item: &ItemMod| -> bool {
            check_explore_module(&should_explore_module, &mut modules_visited, mod_symbol, mod_item)
        };

        walk_module_items(ast, dir, crate_symbol, &mut visit, &mut explore_module, &emit_warning)
            .ok()
            .unwrap();
    }

    #[test]
    fn test_walk_module_and_symbols_type() {
        let module_skip: ItemPath = item_path("crate::skip");

        let source = indoc! { "
            struct AStruct {}

            mod skip {
              struct Skip {}
            }

            mod a {
              mod b {
                trait ATrait {}
              }

              struct FooStruct {}
            }
            "
        };

        let mut symbols_type: HashMap<ItemPath, SymbolType> = HashMap::new();
        let warnings = RefCell::new(Vec::new());

        explore_crate(
            &syn::parse_file(source).unwrap().items,
            &PathBuf::new(),
            &item_path("crate"),
            |m| *m != module_skip,
            &mut symbols_type,
            |msg| warnings.borrow_mut().push(msg.to_owned()),
        );

        let expected: HashMap<ItemPath, SymbolType> = [
            (item_path("crate"), SymbolType::Crate),
            (item_path("crate::AStruct"), SymbolType::Struct),
            (item_path("crate::skip"), SymbolType::Mod),
            (item_path("crate::a"), SymbolType::Mod),
            (item_path("crate::a::b"), SymbolType::Mod),
            (item_path("crate::a::b::ATrait"), SymbolType::Trait),
            (item_path("crate::a::FooStruct"), SymbolType::Struct),
        ]
        .into_iter()
        .collect();

        assert_eq!(symbols_type, expected);
    }

    #[test]
    fn test_symbols_type_with_mod_under_cfg_test() {
        let source = indoc! { "
            #[cfg(not(test))]
            mod a {
              struct MyStruct {}
            }

            #[cfg(test)]
            mod a {
              struct MyStructTest {}
            }

            #[cfg(test)]
            mod b {
              struct MyStructTest {}
            }

            #[cfg(not(test))]
            mod b {
              struct MyStruct {}
            }
            "
        };

        let mut symbols_type: HashMap<ItemPath, SymbolType> = HashMap::new();
        let warnings = RefCell::new(Vec::new());

        explore_crate(
            &syn::parse_file(source).unwrap().items,
            &PathBuf::new(),
            &item_path("crate"),
            |_| true,
            &mut symbols_type,
            |msg| warnings.borrow_mut().push(msg.to_owned()),
        );

        let expected: HashMap<ItemPath, SymbolType> = [
            (item_path("crate"), SymbolType::Crate),
            (item_path("crate::a"), SymbolType::Mod),
            (item_path("crate::a::MyStruct"), SymbolType::Struct),
            (item_path("crate::b"), SymbolType::Mod),
            (item_path("crate::b::MyStruct"), SymbolType::Struct),
        ]
        .into_iter()
        .collect();

        assert_eq!(symbols_type, expected);
    }

    #[test]
    fn test_symbols_type_multiple_module_first_wins() {
        let source = indoc! { "
            #[cfg(not(foo))]
            mod a {
              struct MyStruct {}
            }

            #[cfg(foo)]
            mod a {
              struct Skip {}
            }
            "
        };

        let mut symbols_type: HashMap<ItemPath, SymbolType> = HashMap::new();
        let warnings = RefCell::new(Vec::new());

        explore_crate(
            &syn::parse_file(source).unwrap().items,
            &PathBuf::new(),
            &item_path("crate"),
            |_| true,
            &mut symbols_type,
            |msg| warnings.borrow_mut().push(msg.to_owned()),
        );

        let expected: HashMap<ItemPath, SymbolType> = [
            (item_path("crate"), SymbolType::Crate),
            (item_path("crate::a"), SymbolType::Mod),
            (item_path("crate::a::MyStruct"), SymbolType::Struct),
        ]
        .into_iter()
        .collect();

        assert_eq!(symbols_type, expected);
    }

    #[test]
    fn test_traverse_module_expore_lazily() {
        let symbols: HashSet<ItemPath> = [item_path("crate::module")].into_iter().collect();
        let modules = all_supermodules(symbols.iter());

        let source = indoc! { "
            mod module {
              struct Foo {}
            }
            "
        };

        let mut symbols_type: HashMap<ItemPath, SymbolType> = HashMap::new();
        let warnings = RefCell::new(Vec::new());

        explore_crate(
            &syn::parse_file(source).unwrap().items,
            &PathBuf::new(),
            &item_path("crate"),
            |module| modules.contains(module),
            &mut symbols_type,
            |msg| warnings.borrow_mut().push(msg.to_owned()),
        );

        let symbols_type: HashSet<ItemPath> = symbols_type.keys().cloned().collect();

        // We should still get `crate::module`, but nothing inside it.
        let expected: HashSet<ItemPath> =
            [item_path("crate"), item_path("crate::module")].into_iter().collect();

        assert_eq!(symbols_type, expected);
    }

    #[test]
    fn test_documentation_url() {
        let config = IntralinksDocsRsConfig::default();

        let link = documentation_url(&item_path("crate"), SymbolType::Crate, "foobini", &config);
        assert_eq!(link, "https://docs.rs/foobini/latest/foobini/");

        let link =
            documentation_url(&item_path("crate::AStruct"), SymbolType::Struct, "foobini", &config);
        assert_eq!(link, "https://docs.rs/foobini/latest/foobini/struct.AStruct.html");

        let link =
            documentation_url(&item_path("crate::amodule"), SymbolType::Mod, "foobini", &config);
        assert_eq!(link, "https://docs.rs/foobini/latest/foobini/amodule/");

        let link = documentation_url(&item_path("::std"), SymbolType::Crate, "foobini", &config);
        assert_eq!(link, "https://doc.rust-lang.org/stable/std/");

        let link = documentation_url(
            &item_path("::std::collections::HashMap"),
            SymbolType::Struct,
            "foobini",
            &config,
        );
        assert_eq!(link, "https://doc.rust-lang.org/stable/std/collections/struct.HashMap.html");

        let link = documentation_url(
            &ItemPath::from_string("crate::amodule").unwrap(),
            SymbolType::Mod,
            "foo-bar-mumble",
            &config,
        );
        assert_eq!(link, "https://docs.rs/foo-bar-mumble/latest/foo_bar_mumble/amodule/");

        let config = IntralinksDocsRsConfig {
            docs_rs_base_url: Some("https://docs.company.rs".to_owned()),
            docs_rs_version: Some("1.0.0".to_owned()),
        };

        let link =
            documentation_url(&item_path("crate::Foo"), SymbolType::Struct, "foobini", &config);
        assert_eq!(link, "https://docs.company.rs/foobini/1.0.0/foobini/struct.Foo.html");
    }

    #[test]
    fn test_extract_markdown_intralink_symbols() {
        let doc = indoc! { "
            # Foobini

            This [beautiful crate](crate) is cool because it contains [modules](crate::amodule)
            and some other [stuff](https://en.wikipedia.org/wiki/Stuff) as well.

            Go ahead and check all the [structs in foo](crate::foo#structs).
            Also check [this](::std::sync::Arc) and [this](::alloc::sync::Arc).
            "
        };

        let symbols = extract_markdown_intralink_symbols(&Doc::from_str(doc));

        let expected: HashSet<ItemPath> = [
            item_path("crate"),
            item_path("crate::amodule"),
            item_path("crate::foo"),
            item_path("::std::sync::Arc"),
            item_path("::alloc::sync::Arc"),
        ]
        .into_iter()
        .collect();

        assert_eq!(symbols, expected);
    }

    #[test]
    fn test_rewrite_markdown_links() {
        let doc = indoc! { r"
            # Foobini

            This [beautiful crate](crate) is cool because it contains [modules](crate::amodule)
            and some other [stuff](https://en.wikipedia.org/wiki/Stuff) as well.

            This link is [broken](crate::broken) and this is [not supported](::foo::bar), but this
            should [wor\\k \[fi\]le](f\\i\(n\)e).

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
            &symbols_type,
            "foobini",
            &|_| (),
            &IntralinksConfig::default(),
        );
        let expected = indoc! { r"
            # Foobini

            This [beautiful crate](https://docs.rs/foobini/latest/foobini/) is cool because it contains [modules](https://docs.rs/foobini/latest/foobini/amodule/)
            and some other [stuff](https://en.wikipedia.org/wiki/Stuff) as well.

            This link is broken and this is not supported, but this
            should [wor\\k \[fi\]le](f\\i\(n\)e).

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
            should [wor\\k \[fi\]le](f\\i\(n\)e).

            Go ahead and check all the [structs in foo](crate::foo#structs) specifically
            [this one](crate::foo::BestStruct).  Also, this is a nice function: [copy](::std::fs::copy).

            [![BestStruct doc](https://example.com/image.png)](crate::foo::BestStruct)

            And it works with backtricks as well: [modules](`crate::amodule`).
            "
        };

        let symbols_type: HashMap<ItemPath, SymbolType> = [
            (item_path("crate"), SymbolType::Crate),
            (item_path("crate::amodule"), SymbolType::Mod),
            (item_path("crate::foo"), SymbolType::Mod),
            (item_path("crate::foo::BestStruct"), SymbolType::Struct),
        ]
        .into_iter()
        .collect();

        let new_readme = rewrite_markdown_links(
            &Doc::from_str(doc),
            &symbols_type,
            "foobini",
            &|_| (),
            &IntralinksConfig { strip_links: Some(true), ..std::default::Default::default() },
        );
        let expected = indoc! { r"
            # Foobini

            This beautiful crate is cool because it contains modules
            and some other [stuff](https://en.wikipedia.org/wiki/Stuff) as well.

            This link is broken and this is not supported, but this
            should [wor\\k \[fi\]le](f\\i\(n\)e).

            Go ahead and check all the structs in foo specifically
            this one.  Also, this is a nice function: copy.

            ![BestStruct doc](https://example.com/image.png)

            And it works with backtricks as well: modules.
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
            should [wor\\k \[fi\]le](f\\i\(n\)e).

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
            &symbols_type,
            "foobini",
            &|_| (),
            &IntralinksConfig::default(),
        );
        let expected = indoc! { r"
            # Foobini

            This [beautiful crate](https://docs.rs/foobini/latest/foobini/) is cool because it contains [modules](https://docs.rs/foobini/latest/foobini/amodule/)
            and some other [stuff](https://en.wikipedia.org/wiki/Stuff) as well.

            This link is broken and this is not supported, but this
            should [wor\\k \[fi\]le](f\\i\(n\)e).

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

            This link is [broken] and this is [not supported], [nor this],
            but this should [wor\\k \[fi\]le].

            Go ahead and check all the [structs in foo] specifically
            [this one].  Also, this is a nice function: [copy].

            [![BestStruct doc]][BestStruct]

            [beautiful crate]: crate
            [modules]: crate::amodule
            [stuff]: https://en.wikipedia.org/wiki/Stuff
            [broken]: crate::broken
            [not supported]: ::foo::bar
            [nor this]: crate::foo::BestStruct "title"
            [wor\\k \[fi\]le]: f\\i\(n\)e
            [structs in foo]: crate::foo#structs
            [this one]: crate::foo::BestStruct
            [copy]: ::std::fs::copy
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

        let new_readme = rewrite_reference_definitions(
            &Doc::from_str(doc),
            &symbols_type,
            "foobini",
            &|_| (),
            &IntralinksConfig::default(),
        );
        let expected = indoc! { r#"
            # Foobini

            This [beautiful crate] is cool because it contains [modules]
            and some other [stuff] as well.

            This link is [broken] and this is [not supported], [nor this],
            but this should [wor\\k \[fi\]le].

            Go ahead and check all the [structs in foo] specifically
            [this one].  Also, this is a nice function: [copy].

            [![BestStruct doc]][BestStruct]

            [beautiful crate]: https://docs.rs/foobini/latest/foobini/
            [modules]: https://docs.rs/foobini/latest/foobini/amodule/
            [stuff]: https://en.wikipedia.org/wiki/Stuff
            [broken]: crate::broken
            [not supported]: ::foo::bar
            [nor this]: crate::foo::BestStruct "title"
            [wor\\k \[fi\]le]: f\\i\(n\)e
            [structs in foo]: https://docs.rs/foobini/latest/foobini/foo/#structs
            [this one]: https://docs.rs/foobini/latest/foobini/foo/struct.BestStruct.html
            [copy]: https://doc.rust-lang.org/stable/std/fs/fn.copy.html
            [BestStruct doc]: https://example.com/image.png
            [BestStruct]: https://docs.rs/foobini/latest/foobini/foo/struct.BestStruct.html
        "#
        };

        assert_eq!(new_readme.as_string(), expected);
    }
}
