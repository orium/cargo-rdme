/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use cargo_rdme::find_first_file_in_ancestors;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use thiserror::Error;

const PROJECT_NAME: &'static str = env!("CARGO_PKG_NAME");
const VERSION: &'static str = env!("CARGO_PKG_VERSION");

#[derive(Debug)]
pub struct InvalidOptValue {
    got: String,
    expected: &'static [&'static str],
}

#[derive(Debug)]
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
    // TODO trait for options?
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

#[derive(Debug)]
pub struct CmdOptions {
    line_terminator: Option<LineTerminatorOpt>,
    check: bool,
}

pub fn cmd_options() -> CmdOptions {
    use clap::{App, Arg};

    let cmd_opts = App::new(PROJECT_NAME)
        .version(VERSION)
        .about("Create the README from your crate’s documentation.")
        .version_short("v")
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
        .get_matches();

    let line_terminator: Option<LineTerminatorOpt> = cmd_opts
        .value_of("line-terminator")
        .map(LineTerminatorOpt::from_str)
        .map(|r| r.unwrap_or_else(|e| panic!("this should never happen: {:?}", e)));

    CmdOptions { line_terminator, check: cmd_opts.is_present("check") }
}

#[derive(Error, Debug)]
pub enum ConfigFileOptionsError {
    #[error("failed to read configuration file: {0}")]
    ErrorReadingConfigFile(PathBuf),
    #[error("failed to parse toml")]
    ErrorParsingToml,
    #[error("invalid field \"{0}\"")]
    InvalidField(&'static str),
}

#[derive(Debug)]
pub struct ConfigFileOptions {
    line_terminator: Option<LineTerminatorOpt>,
}

pub fn config_file_options(
    current_dir: impl AsRef<Path>,
) -> Result<Option<ConfigFileOptions>, ConfigFileOptionsError> {
    find_first_file_in_ancestors(current_dir, ".cargo-rdme.toml")
        .map(|file_path| {
            let config_str = std::fs::read_to_string(&file_path)
                .map_err(|_| ConfigFileOptionsError::ErrorReadingConfigFile(file_path))?;
            let config_toml: toml::Value = toml::from_str(&config_str)
                .map_err(|_| ConfigFileOptionsError::ErrorParsingToml)?;

            let line_terminator = config_toml
                .get("line-terminator")
                .map(|v| {
                    v.as_str()
                        .ok_or(ConfigFileOptionsError::InvalidField("line-terminator"))
                        .and_then(|str| {
                            LineTerminatorOpt::from_str(str).map_err(|_| {
                                ConfigFileOptionsError::InvalidField("line-terminator")
                            })
                        })
                })
                .transpose()?;

            Ok(ConfigFileOptions { line_terminator })
        })
        .transpose()
}

#[derive(Debug)]
pub struct Options {
    pub line_terminator: LineTerminatorOpt,
    pub check: bool,
}

pub fn merge_options(
    cmd_options: CmdOptions,
    config_file_options: Option<ConfigFileOptions>,
) -> Options {
    Options {
        line_terminator: cmd_options
            .line_terminator
            .or(config_file_options.and_then(|c| c.line_terminator))
            .unwrap_or(LineTerminatorOpt::default()),
        check: cmd_options.check,
    }
}
