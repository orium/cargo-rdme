/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use std::str::FromStr;

const PROJECT_NAME: &'static str = env!("CARGO_PKG_NAME");
const VERSION: &'static str = env!("CARGO_PKG_VERSION");

#[derive(Debug)]
pub struct InvalidOptValue {
    got: String,
    expected: &'static [&'static str],
}

pub enum LineTerminatorOpt {
    Auto,
    Lf,
    CrLf,
}

impl LineTerminatorOpt {
    const VALUES: &'static [&'static str] = &["auto", "lf", "crlf"];
    const DEFAULT: &'static str = "auto";
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

pub struct Options {
    pub line_terminator: Option<LineTerminatorOpt>,
    pub check: bool,
}

pub fn options_from_cmd() -> Options {
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
                .possible_values(LineTerminatorOpt::VALUES)
                .default_value(LineTerminatorOpt::DEFAULT),
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

    Options { line_terminator, check: cmd_opts.is_present("check") }
}
