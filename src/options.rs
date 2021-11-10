/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use cargo_rdme::find_first_file_in_ancestors;
use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use thiserror::Error;

const PROJECT_NAME: &str = env!("CARGO_PKG_NAME");
const VERSION: &str = env!("CARGO_PKG_VERSION");

#[derive(Debug)]
pub struct InvalidOptValue {
    got: String,
    expected: &'static [&'static str],
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum LineTerminatorOpt {
    Auto,
    Lf,
    CrLf,
}

impl Default for LineTerminatorOpt {
    fn default() -> LineTerminatorOpt {
        LineTerminatorOpt::Auto
    }
}

impl LineTerminatorOpt {
    const VALUES: &'static [&'static str] = &["auto", "lf", "crlf"];
}

impl FromStr for LineTerminatorOpt {
    type Err = InvalidOptValue;

    fn from_str(s: &str) -> Result<LineTerminatorOpt, InvalidOptValue> {
        match s {
            "auto" => Ok(LineTerminatorOpt::Auto),
            "lf" => Ok(LineTerminatorOpt::Lf),
            "crlf" => Ok(LineTerminatorOpt::CrLf),
            _ => Err(InvalidOptValue { got: s.to_owned(), expected: LineTerminatorOpt::VALUES }),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum EntrypointOpt {
    Auto,
    Lib,
    BinDefault,
    BinName(String),
}

impl Default for EntrypointOpt {
    fn default() -> EntrypointOpt {
        EntrypointOpt::Auto
    }
}

#[derive(Debug)]
pub struct CmdOptions {
    entrypoint: Option<EntrypointOpt>,
    line_terminator: Option<LineTerminatorOpt>,
    check: bool,
    force: bool,
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
    use clap::{App, Arg};

    #[allow(clippy::needless_pass_by_value)]
    fn validator_entrypoint(value: String) -> Result<(), String> {
        match value.as_str() {
            "auto" | "lib" | "bin" => Ok(()),
            v if v.starts_with("bin:") && v.len() > "bin:".len() => Ok(()),
            _ => Err(format!("invalid value \"{}\"", value)),
        }
    }

    let cmd_opts = App::new(PROJECT_NAME)
        .version(VERSION)
        .about("Create the README from your crate’s documentation.")
        .version_short("v")
        .arg(
            Arg::with_name("entrypoint")
                .long("entrypoint")
                .help("selects the source code entrypoint of the crate (e.g. auto, lib, bin, bin:<name>)")
                .takes_value(true)
                .validator(validator_entrypoint),
        )
        .arg(
            Arg::with_name("line-terminator")
                .long("line-terminator")
                .help("line terminator to use when writing the README file")
                .takes_value(true)
                .possible_values(LineTerminatorOpt::VALUES),
        )
        .arg(
            Arg::with_name("check")
                .long("check")
                .short("c")
                .help("checks if the README is up to date"),
        )
        .arg(
            Arg::with_name("force")
                .long("force")
                .short("f")
                .help("force README update, even when there are uncommitted changes"),
        )
        .get_matches_from(get_cmd_args());

    let line_terminator: Option<LineTerminatorOpt> = cmd_opts
        .value_of("line-terminator")
        .map(LineTerminatorOpt::from_str)
        .map(|r| r.unwrap_or_else(|e| panic!("this should never happen: {:?}", e)));

    let entrypoint = match cmd_opts.value_of("entrypoint") {
        Some("auto") => Some(EntrypointOpt::Auto),
        Some("lib") => Some(EntrypointOpt::Lib),
        Some("bin") => Some(EntrypointOpt::BinDefault),
        Some(bin_name) => {
            assert!(bin_name.starts_with("bin:"), "clap should validate this");

            let name = bin_name["bin:".len()..].to_owned();

            assert!(!name.is_empty(), "clap should validate this");

            Some(EntrypointOpt::BinName(name))
        }
        None => None,
    };

    CmdOptions {
        entrypoint,
        line_terminator,
        check: cmd_opts.is_present("check"),
        force: cmd_opts.is_present("force"),
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
    entrypoint: Option<EntrypointOpt>,
    line_terminator: Option<LineTerminatorOpt>,
}

fn config_file_options_from_str(
    config_str: &str,
) -> Result<ConfigFileOptions, ConfigFileOptionsError> {
    let config_toml: toml::Value =
        toml::from_str(config_str).map_err(ConfigFileOptionsError::ErrorParsingToml)?;

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

    Ok(ConfigFileOptions { entrypoint, line_terminator })
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
    pub entrypoint: EntrypointOpt,
    pub line_terminator: LineTerminatorOpt,
    pub check: bool,
    pub force: bool,
}

#[allow(clippy::needless_pass_by_value)]
pub fn merge_options(
    cmd_options: CmdOptions,
    config_file_options: Option<ConfigFileOptions>,
) -> Options {
    Options {
        entrypoint: cmd_options
            .entrypoint
            .or_else(|| config_file_options.as_ref().and_then(|c| c.entrypoint.clone()))
            .unwrap_or_default(),
        line_terminator: cmd_options
            .line_terminator
            .or_else(|| config_file_options.as_ref().and_then(|c| c.line_terminator))
            .unwrap_or_default(),
        check: cmd_options.check,
        force: cmd_options.force,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_config_file_options_from_str() {
        let str = indoc! { r#"
            line-terminator = "crlf"

            [entrypoint]
            type = "bin"
            bin-name = "baz"
            "#
        };

        let config_file_opts = config_file_options_from_str(str).unwrap();

        let expected = ConfigFileOptions {
            entrypoint: Some(EntrypointOpt::BinName("baz".to_owned())),
            line_terminator: Some(LineTerminatorOpt::CrLf),
        };

        assert_eq!(config_file_opts, expected);
    }

    #[test]
    fn test_merge_cmd_wins_over_config_file() {
        let cmd_options = CmdOptions {
            entrypoint: Some(EntrypointOpt::BinDefault),
            line_terminator: Some(LineTerminatorOpt::CrLf),
            check: true,
            force: true,
        };
        let config_file_options = ConfigFileOptions {
            entrypoint: Some(EntrypointOpt::Lib),
            line_terminator: Some(LineTerminatorOpt::Lf),
        };

        let options = merge_options(cmd_options, Some(config_file_options));

        let expected = Options {
            entrypoint: EntrypointOpt::BinDefault,
            line_terminator: LineTerminatorOpt::CrLf,
            check: true,
            force: true,
        };

        assert_eq!(options, expected);
    }
}
