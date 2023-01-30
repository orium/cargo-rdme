/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use std::path::{Path, PathBuf};
use syn::Ident;
use syn::Item;
use thiserror::Error;

use super::ItemPath;

#[derive(Error, Debug)]
pub enum ModuleWalkError {
    #[error("IO error: {0}")]
    IOError(std::io::Error),
    #[error("failed to parse rust file: {0}")]
    ParseError(syn::Error),
}

impl From<std::io::Error> for ModuleWalkError {
    fn from(err: std::io::Error) -> Self {
        ModuleWalkError::IOError(err)
    }
}

impl From<syn::Error> for ModuleWalkError {
    fn from(err: syn::Error) -> Self {
        ModuleWalkError::ParseError(err)
    }
}

fn file_ast<P: AsRef<Path>>(filepath: P) -> Result<syn::File, ModuleWalkError> {
    let src = std::fs::read_to_string(filepath)?;

    Ok(syn::parse_file(&src)?)
}

/// Determines the module filename, which can be `<module>.rs` or `<module>/mod.rs`.
fn module_filename(dir: &Path, module: &Ident) -> Option<PathBuf> {
    let mod_file = dir.join(format!("{}.rs", module));

    if mod_file.is_file() {
        return Some(mod_file);
    }

    let mod_file = dir.join(module.to_string()).join("mod.rs");

    if mod_file.is_file() {
        return Some(mod_file);
    }

    None
}

pub(super) fn walk_module_items(
    ast: &[Item],
    dir: &Path,
    mod_symbol: &ItemPath,
    visit: &mut impl FnMut(&ItemPath, &Item),
    explore_module: &mut impl FnMut(&ItemPath, &syn::ItemMod) -> bool,
    emit_warning: &impl Fn(&str),
) -> Result<(), ModuleWalkError> {
    for item in ast.iter() {
        visit(mod_symbol, item);

        if let Item::Mod(module) = item {
            let child_module_symbol: ItemPath = mod_symbol.clone().join(&module.ident.to_string());

            if explore_module(&child_module_symbol, module) {
                match &module.content {
                    Some((_, items)) => {
                        walk_module_items(
                            items,
                            dir,
                            &child_module_symbol,
                            visit,
                            explore_module,
                            emit_warning,
                        )?;
                    }
                    None => match module_filename(dir, &module.ident) {
                        None => emit_warning(&format!(
                            "Unable to find module file for module {} in directory \"{}\"",
                            child_module_symbol,
                            dir.display()
                        )),
                        Some(mod_filename) => walk_module_file(
                            mod_filename,
                            &child_module_symbol,
                            visit,
                            explore_module,
                            emit_warning,
                        )?,
                    },
                }
            }
        }
    }

    Ok(())
}

pub(super) fn walk_module_file<P: AsRef<Path>>(
    file: P,
    mod_symbol: &ItemPath,
    visit: &mut impl FnMut(&ItemPath, &Item),
    explore_module: &mut impl FnMut(&ItemPath, &syn::ItemMod) -> bool,
    emit_warning: &impl Fn(&str),
) -> Result<(), ModuleWalkError> {
    let dir: &Path = file
        .as_ref()
        .parent()
        .unwrap_or_else(|| panic!("failed to get directory of \"{}\"", file.as_ref().display()));
    let ast: syn::File = file_ast(&file)?;

    walk_module_items(&ast.items, dir, mod_symbol, visit, explore_module, emit_warning)
}
