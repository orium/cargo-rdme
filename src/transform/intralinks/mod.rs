use crate::Doc;
use crate::transform::DocTransform;
use crate::transform::intralinks::links::{
    Link, MarkdownLink, markdown_link_iterator, markdown_reference_link_definition_iterator,
};
use module_walker::walk_module_file;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fmt::Write;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use syn::{Item, ItemMod};
use thiserror::Error;
use unicase::UniCase;

mod links;
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
        let mut symbols: HashSet<ItemPath> = extract_markdown_intralink_symbols(doc);

        let strip_links = self.config.strip_links.unwrap_or(false);

        // Also extract shorthand [`code`] patterns and add them as potential symbols.
        let shorthand_symbols = extract_shorthand_symbols(doc);
        symbols.extend(shorthand_symbols);

        // If there are no links at all and we're not stripping, nothing to do.
        if symbols.is_empty() && !strip_links {
            return Ok(doc.clone());
        }

        // We only load symbols type information when we need them.
        let symbols_type = match strip_links {
            false => load_symbols_type(&self.entrypoint, &symbols, &self.emit_warning)?,
            true => HashMap::new(),
        };

        let doc =
            rewrite_links(doc, &symbols_type, &self.crate_name, &self.emit_warning, &self.config);

        // Post-process the document to handle shorthand [`code`] patterns.
        // This must handle standalone patterns but skip patterns that are already part of links.
        let doc = post_process_shorthand_links(
            &doc,
            &symbols_type,
            &self.crate_name,
            &self.emit_warning,
            &self.config,
            strip_links,
        );

        Ok(doc)
    }
}

/// Iterator that yields the content between [`...`] patterns that are NOT markdown links.
struct ShorthandPatternIterator<'a> {
    remaining: &'a str,
}

impl<'a> Iterator for ShorthandPatternIterator<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            // Find the next potential pattern.
            let start_idx = self.remaining.find("[`")?;
            let after_start = &self.remaining[start_idx + 2..];

            // Find the closing pattern.
            if let Some(end_idx) = after_start.find("`]") {
                let content = &after_start[..end_idx];
                let after_pattern = &after_start[end_idx + 2..];

                // Check if this is followed by a link URL (indicating it's already a markdown link).
                let is_markdown_link = after_pattern.starts_with('(');

                // Move past this pattern for next iteration.
                self.remaining = &after_start[end_idx + 2..];

                // Only return if not empty and not already a markdown link.
                if !content.is_empty() && !is_markdown_link {
                    return Some(content);
                }
            } else {
                // No valid closing, skip this "[`" and continue.
                self.remaining = &self.remaining[start_idx + 2..];
            }
        }
    }
}

/// Find all [`code`] shorthand patterns in the text that are NOT markdown links.
fn find_shorthand_patterns(text: &str) -> ShorthandPatternIterator<'_> {
    ShorthandPatternIterator { remaining: text }
}

/// Extract shorthand [`code`] patterns and return them as `ItemPaths`.
fn extract_shorthand_symbols(doc: &Doc) -> HashSet<ItemPath> {
    find_shorthand_patterns(doc.as_string())
        .filter_map(|inner| {
            // Remove trailing () if present for function names.
            let clean_name = inner.trim_end_matches("()");
            // Add as a crate-relative symbol.
            ItemPath::from_string(&format!("crate::{clean_name}"))
        })
        .collect()
}

/// Post-process the entire document to handle shorthand [`code`] patterns.
fn post_process_shorthand_links(
    doc: &Doc,
    symbols_type: &HashMap<ItemPath, SymbolType>,
    crate_name: &str,
    emit_warning: &impl Fn(&str),
    config: &IntralinksConfig,
    strip_links: bool,
) -> Doc {
    let doc_str = doc.as_string();

    // Collect patterns with their positions using an iterator.
    let patterns: Vec<(usize, usize, &str)> = std::iter::from_fn({
        let mut offset = 0;
        let mut remaining = doc_str;

        move || {
            while let Some(start_idx) = remaining.find("[`") {
                let after_start = &remaining[start_idx + 2..];

                if let Some(end_idx) = after_start.find("`]") {
                    let content = &after_start[..end_idx];
                    let absolute_start = offset + start_idx;
                    let absolute_end = absolute_start + 2 + end_idx + 2;

                    // Check if this is followed by a link URL (indicating it's already a markdown link).
                    let after_pattern = &remaining[start_idx + 2 + end_idx + 2..];
                    let is_markdown_link = after_pattern.starts_with('(');

                    let consumed = start_idx + 2 + end_idx + 2;
                    remaining = &remaining[consumed..];
                    offset += consumed;

                    // Only return if not empty and not already a markdown link.
                    if !content.is_empty() && !is_markdown_link {
                        return Some((absolute_start, absolute_end, content));
                    }
                } else {
                    let consumed = start_idx + 2;
                    remaining = &remaining[consumed..];
                    offset += consumed;
                }
            }
            None
        }
    })
    .collect();

    // Build the result string by processing each pattern using iterators.
    let result = std::iter::once(0)
        .chain(patterns.iter().map(|(_, end, _)| *end))
        .zip(patterns.iter().map(|(start, _, _)| *start).chain(std::iter::once(doc_str.len())))
        .zip(patterns.iter().map(Some).chain(std::iter::once(None)))
        .map(|((last_end, start), pattern_opt)| {
            let before_text = &doc_str[last_end..start];

            match pattern_opt {
                Some(&(pattern_start, pattern_end, inner)) => {
                    let pattern_replacement = if strip_links {
                        // When stripping, just output the code without brackets.
                        format!("`{inner}`")
                    } else {
                        // Try to generate a docs.rs link.
                        let clean_name = inner.trim_end_matches("()");
                        let temp_link = Link::from(format!("crate::{clean_name}"));

                        let markdown_link_action = markdown_link(
                            &temp_link,
                            symbols_type,
                            crate_name,
                            emit_warning,
                            config,
                        );

                        match markdown_link_action {
                            MarkdownLinkAction::Link(generated_link) => {
                                // Successfully generated a docs.rs link.
                                format!("[`{inner}`]({generated_link})")
                            }
                            MarkdownLinkAction::Strip => {
                                // Unresolvable intralink, strip to just backticked code.
                                format!("`{inner}`")
                            }
                            MarkdownLinkAction::Preserve => {
                                // Not an intralink, keep the original.
                                doc_str[pattern_start..pattern_end].to_string()
                            }
                        }
                    };

                    format!("{before_text}{pattern_replacement}")
                }
                None => before_text.to_string(),
            }
        })
        .collect::<String>();

    Doc::from_str(result)
}

fn rewrite_links(
    doc: &Doc,
    symbols_type: &HashMap<ItemPath, SymbolType>,
    crate_name: &str,
    emit_warning: &impl Fn(&str),
    config: &IntralinksConfig,
) -> Doc {
    let RewriteReferenceLinksResult { doc, reference_links_to_remove } =
        rewrite_reference_links_definitions(doc, symbols_type, crate_name, emit_warning, config);

    // TODO Refactor link removal code so that it all happens in a new phase and not inside the
    //      functions above.
    rewrite_markdown_links(
        &doc,
        symbols_type,
        crate_name,
        emit_warning,
        config,
        &reference_links_to_remove,
    )
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum ItemPathAnchor {
    /// The anchor of a path starting with `::` such as `::std::fs::read`.
    Root,
    /// The anchor of a path starting with `crate` such as `crate::foo::is_prime`.
    Crate,
}

/// The rust path of an item, such as `foo::bar::is_prime` or `crate`.
#[derive(Clone)]
pub struct ItemPath {
    pub anchor: ItemPathAnchor,

    /// This path vector can be shared and can end after the `item_path` we are representing.
    /// This allows us to have a faster implementation for `ItemPath::all_ancestors()`.
    path_shared: Rc<Vec<String>>,
    path_end: usize,
}

impl ItemPath {
    fn new(anchor: ItemPathAnchor) -> ItemPath {
        ItemPath { anchor, path_shared: Rc::new(Vec::new()), path_end: 0 }
    }

    fn root(crate_name: &str) -> ItemPath {
        ItemPath::new(ItemPathAnchor::Root).join(&crate_name)
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

    fn path_components(&self) -> impl Iterator<Item = &str> {
        self.path_shared[0..self.path_end].iter().map(String::as_str)
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

    fn name(&self) -> Option<&str> {
        self.path_end.checked_sub(1).and_then(|i| self.path_shared.get(i)).map(String::as_str)
    }

    fn join(mut self, s: &impl ToString) -> ItemPath {
        let path = Rc::make_mut(&mut self.path_shared);
        path.truncate(self.path_end);
        path.push(s.to_string());
        self.path_end += 1;
        self
    }

    fn all_ancestors(&self) -> impl Iterator<Item = ItemPath> + use<> {
        let first_ancestor = self.clone().parent();

        std::iter::successors(first_ancestor, |ancestor| ancestor.clone().parent())
    }
}

impl PartialEq for ItemPath {
    fn eq(&self, other: &Self) -> bool {
        self.anchor == other.anchor && self.path_components().eq(other.path_components())
    }
}

impl Eq for ItemPath {}

impl Hash for ItemPath {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.anchor.hash(state);
        self.path_components().for_each(|c| c.hash(state));
    }
}

impl fmt::Display for ItemPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self.anchor {
            ItemPathAnchor::Root => (),
            ItemPathAnchor::Crate => f.write_str("crate")?,
        }

        for s in self.path_components() {
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
pub enum ImplSymbolType {
    Method,
    Const,
    Type,
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
    ImplItem(ImplSymbolType),
}

impl SymbolType {
    /// Returns the path of the module where this item is defined.
    ///
    /// Importantly, if inse module `crate::amod` we have a `struct Foo` with method `Foo::method()`,
    /// this will `Foo::method()` return `crate::amod`.
    fn get_module_path(self, path: &ItemPath) -> Option<ItemPath> {
        match self {
            SymbolType::Crate => {
                assert!(path.is_toplevel(), "a crate should always be in a toplevel path");
                None
            }
            SymbolType::Struct
            | SymbolType::Trait
            | SymbolType::Enum
            | SymbolType::Union
            | SymbolType::Type
            | SymbolType::Mod
            | SymbolType::Macro
            | SymbolType::Const
            | SymbolType::Fn
            | SymbolType::Static => {
                let p = path.clone().parent().unwrap_or_else(|| {
                    panic!("item {path} of type {self:?} should have a parent module")
                });
                Some(p)
            }
            SymbolType::ImplItem(_) => {
                let p = path
                    .clone()
                    .parent()
                    .unwrap_or_else(|| {
                        panic!("item {path} of type {self:?} should have a parent type")
                    })
                    .parent()
                    .unwrap_or_else(|| {
                        panic!("item {path} of type {self:?} should have a parent module")
                    });
                Some(p)
            }
        }
    }
}

fn symbols_type_impl_block(
    module: &ItemPath,
    impl_block: &syn::ItemImpl,
) -> Vec<(ItemPath, SymbolType)> {
    use syn::{ImplItem, Type, TypePath};

    if let Type::Path(TypePath { qself: None, path }) = &*impl_block.self_ty {
        if let Some(self_ident) = path.get_ident().map(ToString::to_string) {
            let self_path = module.clone().join(&self_ident);

            return impl_block
                .items
                .iter()
                .filter_map(|item| match item {
                    ImplItem::Fn(m) => {
                        let ident = m.sig.ident.to_string();

                        Some((ident, ImplSymbolType::Method))
                    }
                    ImplItem::Const(c) => {
                        let ident = c.ident.to_string();

                        Some((ident, ImplSymbolType::Const))
                    }
                    ImplItem::Type(t) => {
                        let ident = t.ident.to_string();

                        Some((ident, ImplSymbolType::Type))
                    }
                    _ => None,
                })
                .map(|(ident, tpy)| (self_path.clone().join(&ident), SymbolType::ImplItem(tpy)))
                .collect();
        }
    }

    Vec::new()
}

fn item_symbols_type(module: &ItemPath, item: &Item) -> Vec<(ItemPath, SymbolType)> {
    let item_path = |ident: &syn::Ident| module.clone().join(ident);

    let (path, symbol_type) = match item {
        Item::Enum(e) => (item_path(&e.ident), SymbolType::Enum),
        Item::Struct(s) => (item_path(&s.ident), SymbolType::Struct),
        Item::Trait(t) => (item_path(&t.ident), SymbolType::Trait),
        Item::Union(u) => (item_path(&u.ident), SymbolType::Union),
        Item::Type(t) => (item_path(&t.ident), SymbolType::Type),
        Item::Mod(m) => (item_path(&m.ident), SymbolType::Mod),
        Item::Macro(syn::ItemMacro { ident: Some(ident), .. }) => {
            (item_path(ident), SymbolType::Macro)
        }
        Item::Const(c) => (item_path(&c.ident), SymbolType::Const),
        Item::Fn(f) => (item_path(&f.sig.ident), SymbolType::Fn),
        Item::Static(s) => (item_path(&s.ident), SymbolType::Static),
        Item::Impl(impl_block) => {
            return symbols_type_impl_block(module, impl_block);
        }

        _ => return Vec::new(),
    };

    vec![(path, symbol_type)]
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
    for (symbol, symbol_type) in item_symbols_type(module, item) {
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
    paths_to_explore: &HashSet<ItemPath>,
    symbols_type: &mut HashMap<ItemPath, SymbolType>,
    emit_warning: &impl Fn(&str),
) -> Result<(), module_walker::ModuleWalkError> {
    let mut modules_visited: HashSet<ItemPath> = HashSet::new();

    // Walking the module only visits items, which means we need to add the root `crate` explicitly.
    symbols_type.insert(crate_symbol.clone(), SymbolType::Crate);

    let mut visit = |module: &ItemPath, item: &Item| {
        let save_symbol = |symbol: &ItemPath| {
            // We also check if it belongs to the paths to explore because of impl items (e.g. a
            // method `Foo::method` we need to know about `Foo` type.  For instance if `Foo` is a
            // struct then the link will be `⋯/struct.Foo.html#method.method`.
            symbols.contains(symbol) || paths_to_explore.contains(symbol)
        };

        visit_module_item(save_symbol, symbols_type, module, item);
    };

    let mut explore_module = |mod_symbol: &ItemPath, mod_item: &ItemMod| -> bool {
        check_explore_module(
            |mod_symbol| paths_to_explore.contains(mod_symbol),
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
    let paths_to_explore: HashSet<ItemPath> = all_ancestor_paths(symbols.iter());
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
            &paths_to_explore,
            &mut symbols_type,
            emit_warning,
        )?;
    }

    explore_crate(
        entry_point,
        &ItemPath::new(ItemPathAnchor::Crate),
        symbols,
        &paths_to_explore,
        &mut symbols_type,
        emit_warning,
    )?;

    Ok(symbols_type)
}

/// Create a set with all ancestor paths of `symbols`.  For instance, if `symbols` is
/// `{crate::foo::bar::baz, crate::baz::mumble}` it will return
/// `{crate, crate::foo, crate::foo::bar, crate::baz}`.
fn all_ancestor_paths<'a>(symbols: impl Iterator<Item = &'a ItemPath>) -> HashSet<ItemPath> {
    symbols.into_iter().flat_map(ItemPath::all_ancestors).collect()
}

fn extract_markdown_intralink_symbols(doc: &Doc) -> HashSet<ItemPath> {
    let item_paths_inline_links =
        markdown_link_iterator(&doc.markdown).items().filter_map(|l| match l {
            MarkdownLink::Inline { link: inline_link } => inline_link.link.link_as_item_path(),
            MarkdownLink::Reference { .. } => None,
        });

    let item_paths_reference_link_def = markdown_reference_link_definition_iterator(&doc.markdown)
        .items()
        .filter_map(|l| l.link.link_as_item_path());

    item_paths_inline_links.chain(item_paths_reference_link_def).collect()
}

/// Returns the url for the item.
///
/// This returns `None` if the item type(s) was not successfully resolved.
fn documentation_url(
    item_path: &ItemPath,
    symbols_type: &HashMap<ItemPath, SymbolType>,
    crate_name: &str,
    fragment: Option<&str>,
    config: &IntralinksDocsRsConfig,
) -> Option<String> {
    let package_name = crate_name.replace('-', "_");
    let typ = *symbols_type.get(item_path)?;

    let mut link = match item_path.anchor {
        ItemPathAnchor::Root => {
            let std_crate_name =
                item_path.path_components().next().expect("a root path should not be empty");
            format!("https://doc.rust-lang.org/stable/{std_crate_name}/")
        }
        ItemPathAnchor::Crate => {
            let base_url =
                config.docs_rs_base_url.as_ref().map_or("https://docs.rs", String::as_str);
            let version = config.docs_rs_version.as_ref().map_or("latest", String::as_str);

            format!("{base_url}/{crate_name}/{version}/{package_name}/")
        }
    };

    if typ == SymbolType::Crate {
        return Some(format!("{}{}", link, fragment.unwrap_or("")));
    }

    let skip_components = match item_path.anchor {
        ItemPathAnchor::Root => 1,
        ItemPathAnchor::Crate => 0,
    };

    let module_path = typ.get_module_path(item_path).expect("item should belong to a module");

    for s in module_path.path_components().skip(skip_components) {
        link.push_str(s);
        link.push('/');
    }

    let name =
        item_path.name().unwrap_or_else(|| panic!("failed to get last component of {item_path}"));

    match typ {
        SymbolType::Crate => unreachable!(),
        SymbolType::Struct => write!(&mut link, "struct.{name}.html"),
        SymbolType::Trait => write!(&mut link, "trait.{name}.html"),
        SymbolType::Enum => write!(&mut link, "enum.{name}.html"),
        SymbolType::Union => write!(&mut link, "union.{name}.html"),
        SymbolType::Type => write!(&mut link, "type.{name}.html"),
        SymbolType::Mod => write!(&mut link, "{name}/"),
        SymbolType::Macro => write!(&mut link, "macro.{name}.html"),
        SymbolType::Const => write!(&mut link, "const.{name}.html"),
        SymbolType::Fn => write!(&mut link, "fn.{name}.html"),
        SymbolType::Static => write!(&mut link, "static.{name}.html"),
        SymbolType::ImplItem(typ) => {
            let parent_path = item_path
                .clone()
                .parent()
                .unwrap_or_else(|| panic!("item {item_path} should always have a parent"));

            let link = documentation_url(
                &parent_path,
                symbols_type,
                crate_name,
                // We discard the fragment.
                None,
                config,
            )?;

            let impl_item_fragment_str = match typ {
                ImplSymbolType::Method => "method",
                ImplSymbolType::Const => "associatedconstant",
                ImplSymbolType::Type => "associatedtype",
            };

            return Some(format!("{link}#{impl_item_fragment_str}.{name}"));
        }
    }
    .expect("this should never fail");

    Some(format!("{}{}", link, fragment.unwrap_or("")))
}

enum MarkdownLinkAction {
    Link(Link),
    Preserve,
    Strip,
}

fn markdown_link(
    link: &Link,
    symbols_type: &HashMap<ItemPath, SymbolType>,
    crate_name: &str,
    emit_warning: &impl Fn(&str),
    config: &IntralinksConfig,
) -> MarkdownLinkAction {
    match link.link_as_item_path() {
        Some(symbol) => {
            let link = documentation_url(
                &symbol,
                symbols_type,
                crate_name,
                link.link_fragment(),
                &config.docs_rs,
            );

            match link {
                Some(l) => MarkdownLinkAction::Link(l.into()),
                None => {
                    emit_warning(&format!("Could not resolve definition of `{symbol}`."));

                    // This was an intralink, but we were not able to generate a link.
                    MarkdownLinkAction::Strip
                }
            }
        }
        None => MarkdownLinkAction::Preserve,
    }
}

fn rewrite_markdown_links(
    doc: &Doc,
    symbols_type: &HashMap<ItemPath, SymbolType>,
    crate_name: &str,
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
                let markdown_link: MarkdownLinkAction = match strip_links {
                    false => markdown_link(
                        &inline_link.link,
                        symbols_type,
                        crate_name,
                        emit_warning,
                        config,
                    ),
                    true => match inline_link.link.link_as_item_path() {
                        None => MarkdownLinkAction::Preserve,
                        Some(_) => MarkdownLinkAction::Strip,
                    },
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
    symbols_type: &HashMap<ItemPath, SymbolType>,
    crate_name: &str,
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
                let markdown_link: MarkdownLinkAction = match strip_links {
                    false => markdown_link(
                        &link_ref_def.link,
                        symbols_type,
                        crate_name,
                        emit_warning,
                        config,
                    ),
                    true => match link_ref_def.link.link_as_item_path() {
                        None => MarkdownLinkAction::Preserve,
                        Some(_) => MarkdownLinkAction::Strip,
                    },
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

fn get_rustc_sysroot_libraries_dir() -> Result<PathBuf, IntralinkError> {
    use std::process::Command;

    let output = Command::new("rustc")
        .args(["--print=sysroot"])
        .output()
        .map_err(|e| IntralinkError::LoadStdLibError(format!("failed to run rustc: {e}")))?;

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

#[allow(clippy::too_many_lines)]
#[cfg(test)]
mod tests {
    use super::*;
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
        assert_eq!(item_path("crate::foo").join(&"bar"), item_path("crate::foo::bar"),);
        assert_eq!(item_path("::std::foo").join(&"bar"), item_path("::std::foo::bar"),);

        assert_eq!(
            item_path("::std::foo::bar").parent().unwrap().join(&"baz"),
            item_path("::std::foo::baz"),
        );
    }

    #[test]
    fn test_all_ancestor_paths() {
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

        assert_eq!(all_ancestor_paths(symbols.iter()), expected);
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
        let modules = all_ancestor_paths(symbols.iter());

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
            Some(
                "https://docs.rs/foo-bar-mumble/latest/foo_bar_mumble/struct.MyStruct.html#implementations"
            )
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
            Some(
                "https://docs.rs/foo-bar-mumble/latest/foo_bar_mumble/mymod/struct.MyStruct.html#method.a_method"
            )
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
    fn test_extract_markdown_intralink_symbols() {
        let doc = indoc! { "
            # Foobini

            This [beautiful crate](crate) is cool because it contains [modules](crate::amodule)
            and some other [stuff](https://en.wikipedia.org/wiki/Stuff) as well.

            Go ahead and check all the [structs in foo](crate::foo#structs).
            Also check [this](::std::sync::Arc) and [this](::alloc::sync::Arc).

            We also support [reference][style] [links].

            [style]: crate::amodule
            [links]: crate::foo#structs
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
            &symbols_type,
            "foobini",
            &|_| (),
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
            &symbols_type,
            "foobini",
            &|_| (),
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
            &symbols_type,
            "foobini",
            &|_| (),
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
            &symbols_type,
            "foobini",
            &|_| (),
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
            &symbols_type,
            "foobini",
            &|_| (),
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
            &symbols_type,
            "foobini",
            &|_| (),
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
}
