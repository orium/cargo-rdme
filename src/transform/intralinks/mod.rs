/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use crate::markdown::Markdown;
use crate::transform::utils::MarkdownItemIterator;
use crate::transform::DocTransform;
use crate::{Doc, Project};
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

pub struct DocTransformIntralinks<F> {
    crate_name: String,
    entrypoint: PathBuf,
    emit_warning: F,
}

impl<F> DocTransformIntralinks<F>
where
    F: Fn(&str),
{
    pub fn new(
        crate_name: impl Into<String>,
        entrypoint: impl AsRef<Path>,
        emit_warning: F,
    ) -> DocTransformIntralinks<F> {
        DocTransformIntralinks {
            crate_name: crate_name.into(),
            entrypoint: entrypoint.as_ref().to_path_buf(),
            emit_warning,
        }
    }
}

impl<F> DocTransform for DocTransformIntralinks<F>
where
    F: Fn(&str),
{
    type E = IntralinkError;

    fn transform(&self, doc: &Doc) -> Result<Doc, IntralinkError> {
        let symbols: HashSet<FQIdentifier> = extract_markdown_intralink_symbols(doc);

        // If there are no intralinks in the doc don't even bother doing anything else.
        if symbols.is_empty() {
            return Ok(doc.clone());
        }

        let symbols_type = load_symbols_type(&self.entrypoint, &symbols, &self.emit_warning)?;
        let new_doc =
            rewrite_markdown_links(doc, &symbols_type, &self.crate_name, &self.emit_warning);

        Ok(new_doc)
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub(self) enum FQIdentifierAnchor {
    Root,
    Crate,
}

/// Fully qualified identifier.
#[derive(Clone)]
pub(self) struct FQIdentifier {
    pub anchor: FQIdentifierAnchor,

    /// This path vector can be shared and can end after the identifier we are representing.
    /// This allow us to have a faster implementation for `FQIdentifier::all_ancestors()`.
    path_shared: Rc<Vec<String>>,
    path_end: usize,
}

impl FQIdentifier {
    fn new(anchor: FQIdentifierAnchor) -> FQIdentifier {
        FQIdentifier { anchor, path_shared: Rc::new(Vec::new()), path_end: 0 }
    }

    fn root(crate_name: &str) -> FQIdentifier {
        FQIdentifier::new(FQIdentifierAnchor::Root).join(crate_name)
    }

    fn from_string(s: &str) -> Option<FQIdentifier> {
        let anchor;
        let rest;

        if let Some(r) = s.strip_prefix("::") {
            anchor = FQIdentifierAnchor::Root;
            rest = r;
        } else if s == "crate" {
            return Some(FQIdentifier::new(FQIdentifierAnchor::Crate));
        } else if let Some(r) = s.strip_prefix("crate::") {
            anchor = FQIdentifierAnchor::Crate;
            rest = r;
        } else {
            return None;
        }

        if rest.is_empty() {
            return None;
        }

        let path: Rc<Vec<String>> = Rc::new(rest.split("::").map(str::to_owned).collect());

        Some(FQIdentifier { anchor, path_end: path.len(), path_shared: path })
    }

    fn path(&self) -> &[String] {
        &self.path_shared[0..self.path_end]
    }

    fn is_toplevel(&self) -> bool {
        match self.anchor {
            FQIdentifierAnchor::Root => self.path_end <= 1,
            FQIdentifierAnchor::Crate => self.path_end == 0,
        }
    }

    fn parent(mut self) -> Option<FQIdentifier> {
        match self.is_toplevel() {
            true => None,
            false => {
                self.path_end -= 1;
                Some(self)
            }
        }
    }

    fn join(mut self, s: &str) -> FQIdentifier {
        let path = Rc::make_mut(&mut self.path_shared);
        path.truncate(self.path_end);
        path.push(s.to_string());
        self.path_end += 1;
        self
    }

    fn all_ancestors(&self) -> impl Iterator<Item = FQIdentifier> {
        let first_ancestor = self.clone().parent();

        std::iter::successors(first_ancestor, |ancestor| ancestor.clone().parent())
    }
}

impl PartialEq for FQIdentifier {
    fn eq(&self, other: &Self) -> bool {
        self.anchor == other.anchor && self.path() == other.path()
    }
}

impl Eq for FQIdentifier {}

impl Hash for FQIdentifier {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.anchor.hash(state);
        self.path().hash(state);
    }
}

impl fmt::Display for FQIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self.anchor {
            FQIdentifierAnchor::Root => (),
            FQIdentifierAnchor::Crate => f.write_str("crate")?,
        }

        for s in self.path().iter() {
            f.write_str("::")?;
            f.write_str(s)?;
        }

        Ok(())
    }
}

impl fmt::Debug for FQIdentifier {
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

fn symbol_type(module: &FQIdentifier, item: &Item) -> Option<(FQIdentifier, SymbolType)> {
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
    save_symbol: impl Fn(&FQIdentifier) -> bool,
    symbols_type: &mut HashMap<FQIdentifier, SymbolType>,
    module: &FQIdentifier,
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
    should_explore_module: impl Fn(&FQIdentifier) -> bool,
    modules_visited: &mut HashSet<FQIdentifier>,
    mod_symbol: &FQIdentifier,
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
    crate_symbol: &FQIdentifier,
    symbols: &HashSet<FQIdentifier>,
    modules_to_explore: &HashSet<FQIdentifier>,
    symbols_type: &mut HashMap<FQIdentifier, SymbolType>,
    emit_warning: &impl Fn(&str),
) -> Result<(), module_walker::ModuleWalkError> {
    let mut modules_visited: HashSet<FQIdentifier> = HashSet::new();

    // Walking the module only visits items, which means we need to add the root `crate` explicitly.
    symbols_type.insert(crate_symbol.clone(), SymbolType::Crate);

    let mut visit = |module: &FQIdentifier, item: &Item| {
        visit_module_item(|symbol| symbols.contains(symbol), symbols_type, module, item);
    };

    let mut explore_module = |mod_symbol: &FQIdentifier, mod_item: &ItemMod| -> bool {
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
    symbols: &HashSet<FQIdentifier>,
    emit_warning: &impl Fn(&str),
) -> Result<HashMap<FQIdentifier, SymbolType>, IntralinkError> {
    let modules_to_explore: HashSet<FQIdentifier> = all_supermodules(symbols.iter());
    let mut symbols_type: HashMap<FQIdentifier, SymbolType> = HashMap::new();

    // Only load standard library information if needed.
    let std_lib_crates = match references_standard_library(symbols) {
        true => get_standard_libraries()?,
        false => Vec::new(),
    };

    for Crate { name, entrypoint } in std_lib_crates {
        explore_crate(
            entrypoint,
            &FQIdentifier::root(&name),
            symbols,
            &modules_to_explore,
            &mut symbols_type,
            emit_warning,
        )?;
    }

    explore_crate(
        entry_point,
        &FQIdentifier::new(FQIdentifierAnchor::Crate),
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
fn all_supermodules<'a>(symbols: impl Iterator<Item = &'a FQIdentifier>) -> HashSet<FQIdentifier> {
    symbols.into_iter().flat_map(FQIdentifier::all_ancestors).collect()
}

#[derive(Eq, PartialEq, Clone, Debug)]
struct MarkdownInlineLink {
    text: String,
    link: String,
}

fn split_link_fragment(link: &str) -> (&str, &str) {
    match link.find('#') {
        None => (link, ""),
        Some(i) => link.split_at(i),
    }
}

#[allow(clippy::needless_lifetimes)]
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
            let link = source[(end_text + 2)..(range.end - 1)].to_owned();

            Some((range.into(), MarkdownInlineLink { text, link }))
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

fn extract_markdown_intralink_symbols(doc: &Doc) -> HashSet<FQIdentifier> {
    let mut symbols = HashSet::new();

    for MarkdownInlineLink { link, .. } in markdown_inline_link_iterator(&doc.markdown).items() {
        let (link, _) = split_link_fragment(&link);

        if let Some(symbol) = FQIdentifier::from_string(link) {
            symbols.insert(symbol);
        }
    }

    symbols
}

fn documentation_url(symbol: &FQIdentifier, typ: SymbolType, crate_name: &str) -> String {
    let package_name = crate_name.replace("-", "_");
    let mut path = symbol.path();

    let mut link = match symbol.anchor {
        FQIdentifierAnchor::Root => {
            let std_crate_name = &path[0];
            path = &path[1..];
            format!("https://doc.rust-lang.org/stable/{}/", std_crate_name)
        }
        FQIdentifierAnchor::Crate => {
            format!("https://docs.rs/{}/latest/{}/", crate_name, package_name)
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

fn rewrite_markdown_links(
    doc: &Doc,
    symbols_type: &HashMap<FQIdentifier, SymbolType>,
    crate_name: &str,
    emit_warning: &impl Fn(&str),
) -> Doc {
    use crate::transform::utils::ItemOrOther;

    let mut new_doc = String::with_capacity(doc.as_string().len());

    for item_or_other in markdown_inline_link_iterator(&doc.markdown).complete() {
        match item_or_other {
            ItemOrOther::Item(MarkdownInlineLink { text, link }) => {
                let (link, fragment): (&str, &str) = split_link_fragment(&link);

                match FQIdentifier::from_string(link) {
                    Some(symbol) if symbols_type.contains_key(&symbol) => {
                        let typ = symbols_type[&symbol];
                        let new_link = documentation_url(&symbol, typ, crate_name);

                        new_doc.push_str(&format!("[{}]({}{})", text, new_link, fragment));
                    }

                    r => {
                        if let Some(symbol) = r {
                            emit_warning(&format!("Could not find definition of `{}`.", symbol));
                        }

                        new_doc.push_str(&format!("[{}]({}{})", text, link, fragment));
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
        .args(&["--print=sysroot"])
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

fn references_standard_library(symbols: &HashSet<FQIdentifier>) -> bool {
    // The only way to reference standard libraries that we support is with a intra-link of form `::⋯`.
    symbols.iter().any(|symbol| symbol.anchor == FQIdentifierAnchor::Root)
}

fn get_standard_libraries() -> Result<Vec<Crate>, IntralinkError> {
    let libraries_dir = get_rustc_sysroot_libraries_dir()?;
    let mut std_libs = Vec::with_capacity(64);

    for entry in std::fs::read_dir(&libraries_dir)? {
        let entry = entry?;
        let project_dir_path = entry.path();
        let cargo_manifest_path = project_dir_path.join("Cargo.toml");
        let lib_entrypoint = project_dir_path.join("src").join("lib.rs");

        if cargo_manifest_path.is_file() && lib_entrypoint.is_file() {
            let crate_name = Project::from_dir(&project_dir_path)
                .map_err(|e| {
                    IntralinkError::LoadStdLibError(format!(
                        "failed to load manifest in \"{}\": {}",
                        cargo_manifest_path.display(),
                        e
                    ))
                })?
                .get_package_name()
                .to_owned();
            let crate_info = Crate { name: crate_name, entrypoint: lib_entrypoint };

            std_libs.push(crate_info);
        }
    }

    Ok(std_libs)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::transform::utils::Span;
    use indoc::indoc;
    use module_walker::walk_module_items;
    use std::cell::RefCell;

    fn identifier(id: &str) -> FQIdentifier {
        FQIdentifier::from_string(id).unwrap()
    }

    #[test]
    fn test_identifier_is_toplevel() {
        assert!(!identifier("crate::baz::mumble").is_toplevel());
        assert!(!identifier("::std::baz::mumble").is_toplevel());
        assert!(!identifier("crate::baz").is_toplevel());
        assert!(!identifier("::std::baz").is_toplevel());
        assert!(identifier("crate").is_toplevel());
        assert!(identifier("::std").is_toplevel());
    }

    #[test]
    fn test_identifier_parent() {
        assert_eq!(identifier("crate::baz::mumble").parent(), Some(identifier("crate::baz")));
        assert_eq!(identifier("::std::baz::mumble").parent(), Some(identifier("::std::baz")));
        assert_eq!(identifier("crate::baz").parent(), Some(identifier("crate")));
        assert_eq!(identifier("::std::baz").parent(), Some(identifier("::std")));
        assert_eq!(identifier("crate").parent(), None);
        assert_eq!(identifier("::std").parent(), None);
    }

    #[test]
    fn test_identifier_join() {
        assert_eq!(identifier("crate::foo").join("bar"), identifier("crate::foo::bar"),);
        assert_eq!(identifier("::std::foo").join("bar"), identifier("::std::foo::bar"),);

        assert_eq!(
            identifier("::std::foo::bar").parent().unwrap().join("baz"),
            identifier("::std::foo::baz"),
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
            MarkdownInlineLink { text: "another".to_owned(), link: "http://foo.com".to_owned() }
        );
        assert_eq!(&markdown.as_string()[start..end], "[another](http://foo.com)");

        assert_eq!(iter.next(), None);

        let markdown = Markdown::from_str("[another](http://foo.com)[another][one]");
        let mut iter = markdown_inline_link_iterator(&markdown).items_with_spans();

        let (Span { start, end }, link) = iter.next().unwrap();
        assert_eq!(
            link,
            MarkdownInlineLink { text: "another".to_owned(), link: "http://foo.com".to_owned() }
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
                link: "http://foo.com/foo(bar)".to_owned(),
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
                link: r"http://foo.\(com\)".to_owned(),
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
                link: "http://foo.com".to_owned(),
            }
        );
        assert_eq!(&markdown.as_string()[start..end], "[link with `code`!](http://foo.com)");

        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_all_supermodules() {
        let symbols = [
            identifier("crate::foo::bar::baz"),
            identifier("crate::baz::mumble"),
            identifier("::std::vec::Vec"),
        ];
        let expected: HashSet<FQIdentifier> = [
            identifier("crate"),
            identifier("crate::foo"),
            identifier("crate::foo::bar"),
            identifier("crate::baz"),
            identifier("::std"),
            identifier("::std::vec"),
        ]
        .into_iter()
        .collect();

        assert_eq!(all_supermodules(symbols.iter()), expected);
    }

    fn explore_crate(
        ast: &Vec<Item>,
        dir: &Path,
        crate_symbol: FQIdentifier,
        should_explore_module: impl Fn(&FQIdentifier) -> bool,
        symbols_type: &mut HashMap<FQIdentifier, SymbolType>,
        emit_warning: impl Fn(&str),
    ) {
        let mut modules_visited: HashSet<FQIdentifier> = HashSet::new();

        symbols_type.insert(crate_symbol.clone(), SymbolType::Crate);

        let mut visit = |module: &FQIdentifier, item: &Item| {
            visit_module_item(|_| true, symbols_type, module, item);
        };

        let mut explore_module = |mod_symbol: &FQIdentifier, mod_item: &ItemMod| -> bool {
            check_explore_module(&should_explore_module, &mut modules_visited, mod_symbol, mod_item)
        };

        walk_module_items(ast, dir, &crate_symbol, &mut visit, &mut explore_module, &emit_warning)
            .ok()
            .unwrap();
    }

    #[test]
    fn test_walk_module_and_symbols_type() {
        let module_skip: FQIdentifier = identifier("crate::skip");

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

        let mut symbols_type: HashMap<FQIdentifier, SymbolType> = HashMap::new();
        let warnings = RefCell::new(Vec::new());

        explore_crate(
            &syn::parse_file(&source).unwrap().items,
            &PathBuf::new(),
            identifier("crate"),
            |m| *m != module_skip,
            &mut symbols_type,
            |msg| warnings.borrow_mut().push(msg.to_owned()),
        );

        let expected: HashMap<FQIdentifier, SymbolType> = [
            (identifier("crate"), SymbolType::Crate),
            (identifier("crate::AStruct"), SymbolType::Struct),
            (identifier("crate::skip"), SymbolType::Mod),
            (identifier("crate::a"), SymbolType::Mod),
            (identifier("crate::a::b"), SymbolType::Mod),
            (identifier("crate::a::b::ATrait"), SymbolType::Trait),
            (identifier("crate::a::FooStruct"), SymbolType::Struct),
        ]
        .into_iter()
        .collect();

        assert_eq!(symbols_type, expected)
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

        let mut symbols_type: HashMap<FQIdentifier, SymbolType> = HashMap::new();
        let warnings = RefCell::new(Vec::new());

        explore_crate(
            &syn::parse_file(&source).unwrap().items,
            &PathBuf::new(),
            identifier("crate"),
            |_| true,
            &mut symbols_type,
            |msg| warnings.borrow_mut().push(msg.to_owned()),
        );

        let expected: HashMap<FQIdentifier, SymbolType> = [
            (identifier("crate"), SymbolType::Crate),
            (identifier("crate::a"), SymbolType::Mod),
            (identifier("crate::a::MyStruct"), SymbolType::Struct),
            (identifier("crate::b"), SymbolType::Mod),
            (identifier("crate::b::MyStruct"), SymbolType::Struct),
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

        let mut symbols_type: HashMap<FQIdentifier, SymbolType> = HashMap::new();
        let warnings = RefCell::new(Vec::new());

        explore_crate(
            &syn::parse_file(&source).unwrap().items,
            &PathBuf::new(),
            identifier("crate"),
            |_| true,
            &mut symbols_type,
            |msg| warnings.borrow_mut().push(msg.to_owned()),
        );

        let expected: HashMap<FQIdentifier, SymbolType> = [
            (identifier("crate"), SymbolType::Crate),
            (identifier("crate::a"), SymbolType::Mod),
            (identifier("crate::a::MyStruct"), SymbolType::Struct),
        ]
        .into_iter()
        .collect();

        assert_eq!(symbols_type, expected)
    }

    #[test]
    fn test_traverse_module_expore_lazily() {
        let symbols: HashSet<FQIdentifier> = [identifier("crate::module")].into_iter().collect();
        let modules = all_supermodules(symbols.iter());

        let source = indoc! { "
            mod module {
              struct Foo {}
            }
            "
        };

        let mut symbols_type: HashMap<FQIdentifier, SymbolType> = HashMap::new();
        let warnings = RefCell::new(Vec::new());

        explore_crate(
            &syn::parse_file(&source).unwrap().items,
            &PathBuf::new(),
            identifier("crate"),
            |module| modules.contains(module),
            &mut symbols_type,
            |msg| warnings.borrow_mut().push(msg.to_owned()),
        );

        let symbols_type: HashSet<FQIdentifier> = symbols_type.keys().cloned().collect();

        // We should still get `crate::module`, but nothing inside it.
        let expected: HashSet<FQIdentifier> =
            [identifier("crate"), identifier("crate::module")].into_iter().collect();

        assert_eq!(symbols_type, expected);
    }

    #[test]
    fn test_documentation_url() {
        let link = documentation_url(&identifier("crate"), SymbolType::Crate, "foobini");
        assert_eq!(link, "https://docs.rs/foobini/latest/foobini/");

        let link = documentation_url(&identifier("crate::AStruct"), SymbolType::Struct, "foobini");
        assert_eq!(link, "https://docs.rs/foobini/latest/foobini/struct.AStruct.html");

        let link = documentation_url(&identifier("crate::amodule"), SymbolType::Mod, "foobini");
        assert_eq!(link, "https://docs.rs/foobini/latest/foobini/amodule/");

        let link = documentation_url(&identifier("::std"), SymbolType::Crate, "foobini");
        assert_eq!(link, "https://doc.rust-lang.org/stable/std/");

        let link = documentation_url(
            &identifier("::std::collections::HashMap"),
            SymbolType::Struct,
            "foobini",
        );
        assert_eq!(link, "https://doc.rust-lang.org/stable/std/collections/struct.HashMap.html");

        let link = documentation_url(
            &FQIdentifier::from_string("crate::amodule").unwrap(),
            SymbolType::Mod,
            "foo-bar-mumble",
        );
        assert_eq!(link, "https://docs.rs/foo-bar-mumble/latest/foo_bar_mumble/amodule/");
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

        let expected: HashSet<FQIdentifier> = [
            identifier("crate"),
            identifier("crate::amodule"),
            identifier("crate::foo"),
            identifier("::std::sync::Arc"),
            identifier("::alloc::sync::Arc"),
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
            [this one](crate::foo::BestStruct)

            [![BestStruct doc](https://example.com/image.png)](crate::foo::BestStruct)
            "
        };

        let symbols_type: HashMap<FQIdentifier, SymbolType> = [
            (identifier("crate"), SymbolType::Crate),
            (identifier("crate::amodule"), SymbolType::Mod),
            (identifier("crate::foo"), SymbolType::Mod),
            (identifier("crate::foo::BestStruct"), SymbolType::Struct),
        ]
        .into_iter()
        .collect();

        let new_readme =
            rewrite_markdown_links(&Doc::from_str(doc), &symbols_type, "foobini", &mut |_| ());
        let expected = indoc! { r"
            # Foobini

            This [beautiful crate](https://docs.rs/foobini/latest/foobini/) is cool because it contains [modules](https://docs.rs/foobini/latest/foobini/amodule/)
            and some other [stuff](https://en.wikipedia.org/wiki/Stuff) as well.

            This link is [broken](crate::broken) and this is [not supported](::foo::bar), but this
            should [wor\\k \[fi\]le](f\\i\(n\)e).

            Go ahead and check all the [structs in foo](https://docs.rs/foobini/latest/foobini/foo/#structs) specifically
            [this one](https://docs.rs/foobini/latest/foobini/foo/struct.BestStruct.html)

            [![BestStruct doc](https://example.com/image.png)](https://docs.rs/foobini/latest/foobini/foo/struct.BestStruct.html)
            "
        };

        assert_eq!(new_readme.as_string(), expected);
    }
}
