/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

#![allow(clippy::missing_panics_doc)]

use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

pub struct TestOptions {
    pub readme_filename: &'static str,
    pub args: &'static [&'static str],
    pub expected_exit_code: i32,
    pub check_readme_expected: bool,
    pub force: bool,
}

impl Default for TestOptions {
    fn default() -> Self {
        TestOptions {
            readme_filename: "README.md",
            args: &[],
            expected_exit_code: 0,
            check_readme_expected: true,
            force: true,
        }
    }
}

#[must_use]
pub fn test_dir(test_name: &str) -> PathBuf {
    let project_dir = std::env::current_dir().unwrap();
    project_dir.join("tests").join(test_name)
}

#[must_use]
pub fn test_readme_template(test_name: &str) -> PathBuf {
    test_dir(test_name).join("README-template.md")
}

#[must_use]
pub fn test_readme_expected(test_name: &str) -> PathBuf {
    test_dir(test_name).join("README-expected.md")
}

fn is_stderr_terminal() -> bool {
    use std::io::IsTerminal;

    std::io::stderr().is_terminal()
}

fn print_framed(stream: &mut termcolor::Buffer, text: &str) {
    for line in text.lines() {
        writeln!(stream, "┃ {line}").unwrap();
    }
}

fn print_stderr_framed(stream: &mut termcolor::Buffer, stderr: &str) {
    use termcolor::{ColorSpec, WriteColor};

    stream.set_color(ColorSpec::new().set_bold(true)).unwrap();
    writeln!(stream, "┏━━━ stderr ━━━━━━").unwrap();
    stream.reset().unwrap();
    print_framed(stream, stderr);
    writeln!(stream, "┗━━━━━━━━━━━━━━━━━").unwrap();
}

fn print_failure_readme_mismatch(
    expected_readme: &str,
    got_readme: &str,
    readme_path: impl AsRef<Path>,
    expected_readme_path: impl AsRef<Path>,
    stderr: &str,
) {
    use termcolor::{Buffer, Color, ColorSpec, WriteColor};

    let in_ci = std::env::var_os("CI").is_some();
    let mut stream = match is_stderr_terminal() {
        true => Buffer::ansi(),
        false => Buffer::no_color(),
    };

    stream.reset().unwrap();

    stream.set_color(ColorSpec::new().set_bold(true).set_fg(Some(Color::Red))).unwrap();
    write!(stream, "The README doesn’t match what was expected.").unwrap();
    stream.reset().unwrap();
    write!(stream, "\n\n").unwrap();
    stream.set_color(ColorSpec::new().set_bold(true)).unwrap();
    writeln!(stream, "┏━━━ Expected ━━━━").unwrap();
    stream.reset().unwrap();
    print_framed(&mut stream, expected_readme);
    stream.set_color(ColorSpec::new().set_bold(true)).unwrap();
    writeln!(stream, "┠━━━ Got ━━━━━━━━━").unwrap();
    stream.reset().unwrap();
    print_framed(&mut stream, got_readme);
    writeln!(stream, "┗━━━━━━━━━━━━━━━━━").unwrap();

    if !in_ci {
        write!(stream, "\nSee the diff with `").unwrap();
        stream.set_color(ColorSpec::new().set_bold(true)).unwrap();
        write!(
            stream,
            "diff {} {}",
            expected_readme_path.as_ref().display(),
            readme_path.as_ref().display(),
        )
        .unwrap();
        stream.reset().unwrap();
        writeln!(stream, "`.").unwrap();
    }

    if !stderr.is_empty() {
        writeln!(stream).unwrap();
        print_stderr_framed(&mut stream, stderr);
    }

    stream.flush().unwrap();

    eprintln!("{}", std::str::from_utf8(stream.as_slice()).expect("invalid utf-8"));
}

fn print_failure_status_code_mismatch(expected_exit_code: i32, got_exit_code: i32, stderr: &str) {
    use termcolor::{Buffer, Color, ColorSpec, WriteColor};

    let mut stream = match is_stderr_terminal() {
        true => Buffer::ansi(),
        false => Buffer::no_color(),
    };

    stream.reset().unwrap();

    stream.set_color(ColorSpec::new().set_bold(true).set_fg(Some(Color::Red))).unwrap();
    write!(stream, "Expected code {expected_exit_code} but got code {got_exit_code} instead.")
        .unwrap();
    stream.reset().unwrap();
    writeln!(stream).unwrap();

    if !stderr.is_empty() {
        writeln!(stream).unwrap();
        print_stderr_framed(&mut stream, stderr);
    }

    stream.flush().unwrap();

    eprintln!("{}", std::str::from_utf8(stream.as_slice()).expect("invalid utf-8"));
}

const BIN_PATH: &str = env!(concat!("CARGO_BIN_EXE_", env!("CARGO_PKG_NAME")));

pub fn run_test_with_options(test_name: &str, options: &TestOptions) {
    let bin_path = Path::new(BIN_PATH);
    let test_dir = test_dir(test_name);

    assert!(bin_path.is_file(), "Binary not found: {}", bin_path.display());
    assert!(test_dir.is_dir(), "Test directory not found: {}", test_dir.display());

    let expected_readme: PathBuf = test_readme_expected(test_name);
    let template_readme: PathBuf = test_readme_template(test_name);
    let readme = test_dir.join(options.readme_filename);

    if options.check_readme_expected {
        assert!(
            expected_readme.is_file(),
            "Expected readme not found: {}",
            expected_readme.display()
        );
        assert!(
            template_readme.is_file(),
            "Template readme not found: {}",
            template_readme.display()
        );

        std::fs::copy(&template_readme, &readme).unwrap();
    }

    let args: Vec<&str> = {
        let mut args = Vec::from(options.args);

        if options.force {
            args.insert(0, "--force");
        }

        args
    };

    let output = Command::new(bin_path)
        .args(args)
        .current_dir(test_dir)
        .env("RUST_BACKTRACE", "1")
        .output()
        .unwrap_or_else(|_| panic!("Failed to execute {}", bin_path.display()));

    let stderr = String::from_utf8_lossy(&output.stderr);

    let exit_code = output.status.code().expect("no exist code");

    if exit_code != options.expected_exit_code {
        print_failure_status_code_mismatch(options.expected_exit_code, exit_code, &stderr);
        panic!("Test {test_name} failed.");
    }

    if options.check_readme_expected {
        let expected = std::fs::read_to_string(&expected_readme).unwrap();
        let got = std::fs::read_to_string(&readme).unwrap();

        if expected != got {
            print_failure_readme_mismatch(&expected, &got, readme, expected_readme, &stderr);
            panic!("Test {test_name} failed.");
        } else {
            std::fs::remove_file(readme).unwrap();
        }
    }
}

pub fn run_test(test_name: &str) {
    run_test_with_options(test_name, &TestOptions::default());
}
