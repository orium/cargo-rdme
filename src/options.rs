/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use cargo_rdme::find_first_file_in_ancestors;
use cargo_rdme::transform::{IntralinksConfig, IntralinksDocsRsConfig};
use clap::{ArgAction, value_parser};
use std::error::Error;
use std::ffi::OsString;
use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use thiserror::Error;

const PROJECT_NAME: &str = env!("CARGO_PKG_NAME");
const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug)]
pub struct InvalidOptValue {
    value: String,
}

impl Display for InvalidOptValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("invalid value \"{}\"", self.value))
    }
}

impl Error for InvalidOptValue {}

#[derive(PartialEq, Eq, Debug, Default, Clone, Copy)]
pub enum LineTerminatorOpt {
    #[default]
    Auto,
    Lf,
    CrLf,
}

impl FromStr for LineTerminatorOpt {
    type Err = InvalidOptValue;

    fn from_str(s: &str) -> Result<LineTerminatorOpt, InvalidOptValue> {
        match s {
            "auto" => Ok(LineTerminatorOpt::Auto),
            "lf" => Ok(LineTerminatorOpt::Lf),
            "crlf" => Ok(LineTerminatorOpt::CrLf),
            v => Err(InvalidOptValue { value: v.to_owned() }),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Default, Clone)]
pub enum EntrypointOpt {
    #[default]
    Auto,
    Lib,
    BinDefault,
    BinName(String),
}

impl FromStr for EntrypointOpt {
    type Err = InvalidOptValue;

    fn from_str(s: &str) -> Result<EntrypointOpt, InvalidOptValue> {
        match s {
            "auto" => Ok(EntrypointOpt::Auto),
            "lib" => Ok(EntrypointOpt::Lib),
            "bin" => Ok(EntrypointOpt::BinDefault),
            v if v.starts_with("bin:") && v.len() > "bin:".len() => {
                let name = v["bin:".len()..].to_owned();
                Ok(EntrypointOpt::BinName(name))
            }
            v => Err(InvalidOptValue { value: v.to_owned() }),
        }
    }
}

#[derive(Debug)]
pub struct CmdOptions {
    workspace_project: Option<String>,
    entrypoint: Option<EntrypointOpt>,
    line_terminator: Option<LineTerminatorOpt>,
    check: bool,
    no_fail_on_warnings: bool,
    intralinks_strip_links: bool,
    force: bool,
    readme_path: Option<PathBuf>,
    heading_base_level: Option<u8>,
}

fn get_cmd_args() -> Vec<OsString> {
    let mut args: Vec<OsString> = std::env::args_os().collect();
    let subcommand: &str = {
        let package_name = env!("CARGO_PKG_NAME");

        assert!(package_name.starts_with("cargo-"), "package name does not start with `cargo-`");

        &package_name["cargo-".len()..]
    };

    // When cargo executes an external subcommand it passes the name of the command itself as the
    // second argument.  Here we remove that so that we can simply run `cargo run` instead of
    // `cargo run -- rdme` for local development.
    if args.len() >= 2 && args[1] == subcommand {
        args.remove(1);
    }

    args
}

pub fn cmd_options() -> CmdOptions {
    use clap::{Arg, Command};

    let cmd_opts = Command::new(PROJECT_NAME)
        .version(VERSION)
        .about("Create the README from your crate’s documentation.")
        .styles(clap_cargo::style::CLAP_STYLING)
        .arg(
            Arg::new("entrypoint")
                .long("entrypoint")
                .help("selects the source code entrypoint of the crate (e.g. auto, lib, bin, bin:<name>)")
                .value_parser(EntrypointOpt::from_str),
        )
        .arg(
            Arg::new("line-terminator")
                .long("line-terminator")
                .help("line terminator to use when writing the README file")
                .value_parser(LineTerminatorOpt::from_str),
        )
        .arg(
            Arg::new("readme-path")
                .long("readme-path")
                .short('r')
                .help("README file path to use (overrides of what is specified in the project `Cargo.toml`)")
                .value_parser(value_parser!(PathBuf)),
        )
        .arg(
            Arg::new("workspace-project")
                .long("workspace-project")
                .short('w')
                .help("project to get the documentation from if your are using workspaces"),
        )
        .arg(
            Arg::new("check")
                .long("check")
                .short('c')
                .help("checks if the README is up to date (exit code 3 if there’s a mismatch, 4 if warnings were emitted)")
                .action(ArgAction::SetTrue),
        )
        .arg(
        Arg::new("no-fail-on-warnings")
            .long("no-fail-on-warnings")
            .help("do not exit with a error status code when checking if the README is up to date")
            .action(ArgAction::SetTrue),
        )
        .arg(
        Arg::new("intralinks-strip-links")
            .long("intralinks-strip-links")
            .help("remove the intralinks")
            .action(ArgAction::SetTrue),
        )
        .arg(
            Arg::new("heading-base-level")
                .long("heading-base-level")
                .help("heading level to be added to the heading level in the rust documentation")
                .value_parser(value_parser!(u8)),
        )
        .arg(
            Arg::new("force")
                .long("force")
                .short('f')
                .help("force README update, even when there are uncommitted changes")
                .action(ArgAction::SetTrue),
        )
        .get_matches_from(get_cmd_args());

    let workspace_project = cmd_opts.get_one::<String>("workspace-project").cloned();

    let line_terminator: Option<LineTerminatorOpt> =
        cmd_opts.get_one::<LineTerminatorOpt>("line-terminator").copied();

    let entrypoint = cmd_opts.get_one::<EntrypointOpt>("entrypoint").cloned();

    let readme_path = cmd_opts.get_one::<PathBuf>("readme-path").cloned();

    let heading_base_level = cmd_opts.get_one::<u8>("heading-base-level").copied();

    CmdOptions {
        workspace_project,
        entrypoint,
        line_terminator,
        check: cmd_opts.get_flag("check"),
        no_fail_on_warnings: cmd_opts.get_flag("no-fail-on-warnings"),
        intralinks_strip_links: cmd_opts.get_flag("intralinks-strip-links"),
        force: cmd_opts.get_flag("force"),
        readme_path,
        heading_base_level,
    }
}

#[derive(Error, Debug)]
pub enum ConfigFileOptionsError {
    #[error("failed to read configuration file: {0}")]
    ErrorReadingConfigFile(PathBuf),
    #[error("failed to parse toml: {0}")]
    ErrorParsingToml(toml::de::Error),
    #[error("invalid field \"{0}\"")]
    InvalidField(&'static str),
    #[error("invalid entrypoint table")]
    InvalidEntrypointTable,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ConfigFileOptions {
    line_terminator: Option<LineTerminatorOpt>,
    workspace_project: Option<String>,
    entrypoint: Option<EntrypointOpt>,
    readme_path: Option<PathBuf>,
    intralinks: Option<IntralinksConfig>,
    heading_base_level: Option<u8>,
}

fn config_file_options_from_str(
    config_str: &str,
) -> Result<ConfigFileOptions, ConfigFileOptionsError> {
    let config_toml: toml::Value =
        toml::from_str(config_str).map_err(ConfigFileOptionsError::ErrorParsingToml)?;

    let workspace_project =
        config_toml.get("workspace-project").and_then(toml::Value::as_str).map(ToOwned::to_owned);

    let line_terminator = config_toml
        .get("line-terminator")
        .map(|v| {
            v.as_str().ok_or(ConfigFileOptionsError::InvalidField("line-terminator")).and_then(
                |str| {
                    LineTerminatorOpt::from_str(str)
                        .map_err(|_| ConfigFileOptionsError::InvalidField("line-terminator"))
                },
            )
        })
        .transpose()?;

    let entrypoint_table = config_toml.get("entrypoint").and_then(toml::Value::as_table);

    let entrypoint_type =
        entrypoint_table.and_then(|t| t.get("type")).and_then(toml::Value::as_str);
    let entrypoint_bin_name =
        entrypoint_table.and_then(|t| t.get("bin-name")).and_then(toml::Value::as_str);

    let entrypoint = match (entrypoint_type, entrypoint_bin_name) {
        (Some("lib"), None) => Some(EntrypointOpt::Lib),
        (Some("bin"), None) => Some(EntrypointOpt::BinDefault),
        (Some("bin"), Some(name)) => Some(EntrypointOpt::BinName(name.to_owned())),
        (None, None) => None,
        _ => return Err(ConfigFileOptionsError::InvalidEntrypointTable),
    };

    let readme_path =
        config_toml.get("readme-path").and_then(toml::Value::as_str).map(PathBuf::from);

    let heading_base_level: Option<u8> = match config_toml
        .get("heading-base-level")
        .and_then(toml::Value::as_integer)
        .map(TryInto::try_into)
    {
        None => None,
        Some(Ok(l)) => Some(l),
        Some(Err(_)) => return Err(ConfigFileOptionsError::InvalidField("heading-base-level")),
    };

    let intralinks_table = config_toml.get("intralinks").and_then(toml::Value::as_table);

    let intralinks_docs_rs_base_url =
        intralinks_table.and_then(|t| t.get("docs-rs-base-url")).and_then(toml::Value::as_str);
    let intralinks_docs_rs_version =
        intralinks_table.and_then(|t| t.get("docs-rs-version")).and_then(toml::Value::as_str);
    let intralinks_strip_links =
        intralinks_table.and_then(|t| t.get("strip-links")).and_then(toml::Value::as_bool);

    let intralinks = intralinks_table.map(|_| IntralinksConfig {
        docs_rs: IntralinksDocsRsConfig {
            docs_rs_base_url: intralinks_docs_rs_base_url.map(ToOwned::to_owned),
            docs_rs_version: intralinks_docs_rs_version.map(ToOwned::to_owned),
        },
        strip_links: intralinks_strip_links,
    });

    Ok(ConfigFileOptions {
        line_terminator,
        workspace_project,
        entrypoint,
        readme_path,
        intralinks,
        heading_base_level,
    })
}

pub fn config_file_options(
    current_dir: impl AsRef<Path>,
) -> Result<Option<ConfigFileOptions>, ConfigFileOptionsError> {
    find_first_file_in_ancestors(current_dir, ".cargo-rdme.toml")
        .map(|file_path| {
            let config_str = std::fs::read_to_string(&file_path)
                .map_err(|_| ConfigFileOptionsError::ErrorReadingConfigFile(file_path))?;

            config_file_options_from_str(&config_str)
        })
        .transpose()
}

#[derive(PartialEq, Eq, Debug)]
pub struct Options {
    pub workspace_project: Option<String>,
    pub entrypoint: EntrypointOpt,
    pub line_terminator: LineTerminatorOpt,
    pub check: bool,
    pub no_fail_on_warnings: bool,
    pub force: bool,
    pub readme_path: Option<PathBuf>,
    pub intralinks: Option<IntralinksConfig>,
    pub heading_base_level: Option<u8>,
}

#[allow(clippy::needless_pass_by_value)]
pub fn merge_options(
    cmd_options: CmdOptions,
    config_file_options: Option<ConfigFileOptions>,
) -> Options {
    let mut config_file_options = config_file_options;

    Options {
        workspace_project: cmd_options
            .workspace_project
            .or_else(|| config_file_options.as_mut().and_then(|c| c.workspace_project.take())),
        entrypoint: cmd_options
            .entrypoint
            .or_else(|| config_file_options.as_mut().and_then(|c| c.entrypoint.take()))
            .unwrap_or_default(),
        line_terminator: cmd_options
            .line_terminator
            .or_else(|| config_file_options.as_ref().and_then(|c| c.line_terminator))
            .unwrap_or_default(),
        check: cmd_options.check,
        no_fail_on_warnings: cmd_options.no_fail_on_warnings,
        force: cmd_options.force,
        readme_path: cmd_options
            .readme_path
            .or_else(|| config_file_options.as_mut().and_then(|c| c.readme_path.take())),
        intralinks: Some(IntralinksConfig {
            docs_rs: IntralinksDocsRsConfig {
                docs_rs_base_url: config_file_options
                    .as_mut()
                    .and_then(|c| c.intralinks.as_mut())
                    .and_then(|il| il.docs_rs.docs_rs_base_url.take()),
                docs_rs_version: config_file_options
                    .as_mut()
                    .and_then(|c| c.intralinks.as_mut())
                    .and_then(|il| il.docs_rs.docs_rs_version.take()),
            },
            strip_links: match cmd_options.intralinks_strip_links {
                true => Some(true),
                false => config_file_options
                    .as_ref()
                    .and_then(|c| c.intralinks.as_ref())
                    .and_then(|il| il.strip_links),
            },
        }),
        heading_base_level: cmd_options
            .heading_base_level
            .or_else(|| config_file_options.and_then(|c| c.heading_base_level)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cargo_rdme::transform::IntralinksDocsRsConfig;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_config_file_options_from_str() {
        let str = indoc! { r#"
            readme-path = "ReAdMe.md"
            workspace-project = "myproj"
            line-terminator = "crlf"
            heading-base-level = 3

            [entrypoint]
            type = "bin"
            bin-name = "baz"

            [intralinks]
            docs-rs-base-url = "https://internaldocs.rs"
            docs-rs-version = "1.0.0"
            strip-links = true
            "#
        };

        let config_file_opts = config_file_options_from_str(str).unwrap();

        let expected = ConfigFileOptions {
            workspace_project: Some("myproj".to_owned()),
            entrypoint: Some(EntrypointOpt::BinName("baz".to_owned())),
            line_terminator: Some(LineTerminatorOpt::CrLf),
            readme_path: Some(PathBuf::from("ReAdMe.md")),
            intralinks: Some(IntralinksConfig {
                docs_rs: IntralinksDocsRsConfig {
                    docs_rs_base_url: Some("https://internaldocs.rs".to_owned()),
                    docs_rs_version: Some("1.0.0".to_owned()),
                },
                strip_links: Some(true),
            }),
            heading_base_level: Some(3),
        };

        assert_eq!(config_file_opts, expected);
    }

    #[test]
    fn test_merge_cmd_wins_over_config_file() {
        let cmd_options = CmdOptions {
            workspace_project: Some("myproj".to_owned()),
            entrypoint: Some(EntrypointOpt::BinDefault),
            line_terminator: Some(LineTerminatorOpt::CrLf),
            check: true,
            no_fail_on_warnings: true,
            intralinks_strip_links: true,
            force: true,
            readme_path: Some(PathBuf::from("rEaDmE.md")),
            heading_base_level: Some(4),
        };
        let config_file_options = ConfigFileOptions {
            workspace_project: Some("aproj".to_owned()),
            entrypoint: Some(EntrypointOpt::Lib),
            line_terminator: Some(LineTerminatorOpt::Lf),
            readme_path: Some(PathBuf::from("ReAdMe.md")),
            intralinks: Some(IntralinksConfig {
                docs_rs: IntralinksDocsRsConfig {
                    docs_rs_base_url: Some("https://internaldocs.rs".to_owned()),
                    docs_rs_version: Some("1.0.0".to_owned()),
                },
                strip_links: Some(false),
            }),
            heading_base_level: Some(3),
        };

        let options = merge_options(cmd_options, Some(config_file_options));

        let expected = Options {
            workspace_project: Some("myproj".to_owned()),
            entrypoint: EntrypointOpt::BinDefault,
            line_terminator: LineTerminatorOpt::CrLf,
            check: true,
            no_fail_on_warnings: true,
            force: true,
            readme_path: Some(PathBuf::from("rEaDmE.md")),
            intralinks: Some(IntralinksConfig {
                docs_rs: IntralinksDocsRsConfig {
                    docs_rs_base_url: Some("https://internaldocs.rs".to_owned()),
                    docs_rs_version: Some("1.0.0".to_owned()),
                },
                strip_links: Some(true),
            }),
            heading_base_level: Some(4),
        };

        assert_eq!(options, expected);
    }
}
