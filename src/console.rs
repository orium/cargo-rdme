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
    writeln!(stream, " {}", message)
}

pub fn print_error(message: impl Display) {
    let mut stream = StandardStream::stderr(ColorChoice::Auto);

    match is_stderr_terminal() {
        true => {
            print_color(&mut stream, "error", message, Color::Red)
                .expect("error writing to stderr");
        }
        false => {
            eprintln!("error: {}", message);
        }
    }
}
