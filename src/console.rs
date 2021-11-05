/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use std::fmt::Display;
use std::io::Write;
use termcolor::ColorChoice;
use termcolor::WriteColor;
use termcolor::{Color, ColorSpec, StandardStream};

fn is_stderr_terminal() -> bool {
    atty::is(atty::Stream::Stderr)
}

fn print_color(
    stream: &mut StandardStream,
    level: impl Display,
    message: impl Display,
    color: Color,
) -> std::io::Result<()> {
    stream.reset()?;
    stream.set_color(ColorSpec::new().set_bold(true).set_fg(Some(color)))?;
    write!(stream, "{}", level)?;
    stream.set_color(ColorSpec::new().set_bold(true))?;
    write!(stream, ":")?;
    stream.reset()?;
    writeln!(stream, " {}", message)?;
    stream.flush()
}

fn print_nocolor(
    stream: &mut StandardStream,
    level: impl Display,
    message: impl Display,
) -> std::io::Result<()> {
    stream.reset()?;
    write!(stream, "{}: {}", level, message)?;
    stream.flush()
}

pub fn print_stderr(level: impl Display, message: impl Display, color: Color) {
    let mut stream = StandardStream::stderr(ColorChoice::Auto);

    match is_stderr_terminal() {
        true => {
            print_color(&mut stream, level, message, color).expect("error writing to stderr");
        }
        false => {
            print_nocolor(&mut stream, level, message).expect("error writing to stderr");
        }
    }
}

pub fn print_error(message: impl Display) {
    print_stderr("error", message, Color::Red);
}

pub fn print_warning(message: impl Display) {
    print_stderr("warning", message, Color::Yellow);
}
