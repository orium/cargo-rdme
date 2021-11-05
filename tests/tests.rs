/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::Command;

use cargo_rdme::{infer_line_terminator, LineTerminator};

struct TestOptions {
    readme_filename: &'static str,
    args: &'static [&'static str],
    expected_exit_status: i32,
    check_readme_expected: bool,
    force: bool,
}

impl Default for TestOptions {
    fn default() -> Self {
        TestOptions {
            readme_filename: "README.md",
            args: &[],
            expected_exit_status: 0,
            check_readme_expected: true,
            force: true,
        }
    }
}

fn test_dir(test_name: &str) -> PathBuf {
    let project_dir = std::env::current_dir().unwrap();
    project_dir.join("tests").join(test_name)
}

fn test_readme_template(test_name: &str) -> PathBuf {
    test_dir(test_name).join("README-template.md")
}

fn test_readme_expected(test_name: &str) -> PathBuf {
    test_dir(test_name).join("README-expected.md")
}

const BIN_PATH: &'static str = env!(concat!("CARGO_BIN_EXE_", env!("CARGO_PKG_NAME")));

fn run_test_with_options(test_name: &str, options: TestOptions) {
    let bin_path = Path::new(BIN_PATH);
    let test_dir = test_dir(test_name);

    if !bin_path.is_file() {
        panic!("Binary not found: {}", bin_path.display());
    }

    if !test_dir.is_dir() {
        panic!("Test directory not found: {}", test_dir.display());
    }

    let expected_readme: PathBuf = test_readme_expected(test_name);
    let template_readme: PathBuf = test_readme_template(test_name);
    let readme = test_dir.join(options.readme_filename);

    if options.check_readme_expected {
        if !expected_readme.is_file() {
            panic!("Expected readme not found: {}", expected_readme.display());
        }

        if !template_readme.is_file() {
            panic!("Template readme not found: {}", template_readme.display());
        }

        std::fs::copy(&template_readme, &readme).unwrap();
    }

    let args: Vec<&str> = {
        let mut args = Vec::from(options.args);

        if options.force {
            args.insert(0, "--force");
        }

        args
    };

    let output = Command::new(&bin_path)
        .args(args)
        .current_dir(test_dir)
        .output()
        .expect(&format!("Failed to execute {}", bin_path.display()));

    let stderr = String::from_utf8_lossy(&output.stderr);

    if output.status.code() != Some(options.expected_exit_status) {
        panic!(
            "Expected code {} but got code {} instead.\n==== stderr ====\n{}",
            options.expected_exit_status,
            output.status.code().map(|c| c.to_string()).unwrap_or("?".to_string()),
            stderr
        );
    }

    if options.check_readme_expected {
        let expected = std::fs::read_to_string(&expected_readme).unwrap();
        let got = std::fs::read_to_string(&readme).unwrap();

        if expected != got {
            let in_ci = std::env::var_os("CI").is_some();

            let diff_msg = match in_ci {
                true => format!("==== Expected ====\n{}\n==== Got ====\n{}", expected, got),
                false => format!(
                    "See the diff with `diff {} {}`.",
                    readme.display(),
                    expected_readme.display()
                ),
            };

            panic!(
                "The generated README does not match what was expected.\n\n{}\n==== stderr ====\n{}",
                diff_msg, stderr
            );
        } else {
            std::fs::remove_file(readme).unwrap();
        }
    }
}

fn run_test(test_name: &str) {
    run_test_with_options(test_name, TestOptions::default())
}

#[test]
fn system_test_simple_single_marker() {
    run_test("simple_single_marker");
}

#[test]
fn system_test_simple_single_marker_no_footer() {
    run_test("simple_single_marker_no_footer");
}

#[test]
fn system_test_simple_start_end_marker() {
    run_test("simple_start_end_marker");
}

#[test]
fn system_test_custom_lib_path() {
    run_test("custom_lib_path");
}

#[test]
fn system_test_custom_readme_path() {
    let option = TestOptions { readme_filename: "READ-ME.md", ..TestOptions::default() };

    run_test_with_options("custom_readme_path", option);
}

#[test]
fn system_test_line_terminator_crlf() {
    let test_name = "line_terminator_crlf";
    let readme_template = test_readme_template(test_name);
    let readme_expected = test_readme_expected(test_name);

    assert_eq!(infer_line_terminator(readme_template).unwrap(), LineTerminator::CrLf);
    assert_eq!(infer_line_terminator(readme_expected).unwrap(), LineTerminator::CrLf);

    run_test(test_name);
}

#[test]
fn system_test_multiline_doc() {
    run_test("multiline_doc");
}

#[test]
fn system_test_option_cmd_line_terminator_lf() {
    let test_name = "option_cmd_line_terminator_lf";
    let readme_template = test_readme_template(test_name);
    let readme_expected = test_readme_expected(test_name);

    assert_eq!(infer_line_terminator(readme_template).unwrap(), LineTerminator::CrLf);
    assert_eq!(infer_line_terminator(readme_expected).unwrap(), LineTerminator::Lf);

    let option = TestOptions { args: &["--line-terminator", "lf"], ..TestOptions::default() };

    run_test_with_options(test_name, option);
}

#[test]
fn system_test_option_cmd_line_terminator_crlf() {
    let test_name = "option_cmd_line_terminator_crlf";
    let readme_template = test_readme_template(test_name);
    let readme_expected = test_readme_expected(test_name);

    assert_eq!(infer_line_terminator(readme_template).unwrap(), LineTerminator::Lf);
    assert_eq!(infer_line_terminator(readme_expected).unwrap(), LineTerminator::CrLf);

    let option = TestOptions { args: &["--line-terminator", "crlf"], ..TestOptions::default() };

    run_test_with_options(test_name, option);
}

#[test]
fn system_test_option_conf_file_line_terminator_lf() {
    let test_name = "option_conf_file_line_terminator_lf";
    let readme_template = test_readme_template(test_name);
    let readme_expected = test_readme_expected(test_name);

    assert_eq!(infer_line_terminator(readme_template).unwrap(), LineTerminator::CrLf);
    assert_eq!(infer_line_terminator(readme_expected).unwrap(), LineTerminator::Lf);

    run_test(test_name);
}

#[test]
fn system_test_option_conf_file_line_terminator_crlf() {
    let test_name = "option_conf_file_line_terminator_crlf";
    let readme_template = test_readme_template(test_name);
    let readme_expected = test_readme_expected(test_name);

    assert_eq!(infer_line_terminator(readme_template).unwrap(), LineTerminator::Lf);
    assert_eq!(infer_line_terminator(readme_expected).unwrap(), LineTerminator::CrLf);

    run_test(test_name);
}

#[test]
fn system_test_option_cmd_check_ok() {
    let test_name = "option_cmd_check_ok";
    let option = TestOptions {
        args: &["--check"],
        check_readme_expected: false,
        expected_exit_status: 0,
        ..TestOptions::default()
    };

    run_test_with_options(test_name, option);
}

#[test]
fn system_test_option_cmd_check_fail() {
    let test_name = "option_cmd_check_fail";
    let option = TestOptions {
        args: &["--check"],
        check_readme_expected: false,
        expected_exit_status: 2,
        ..TestOptions::default()
    };

    run_test_with_options(test_name, option);
}

#[test]
fn system_test_option_cmd_check_fail_line_terminator() {
    let test_name = "option_cmd_check_fail_line_terminator";

    // First check that the test would pass without the line terminator override.
    let option = TestOptions {
        args: &["--check"],
        check_readme_expected: false,
        expected_exit_status: 0,
        ..TestOptions::default()
    };

    run_test_with_options(test_name, option);

    let option = TestOptions {
        args: &["--check", "--line-terminator", "crlf"],
        check_readme_expected: false,
        expected_exit_status: 2,
        ..TestOptions::default()
    };

    run_test_with_options(test_name, option);
}

#[test]
fn system_test_entrypoint_bin_crate() {
    run_test("entrypoint_bin_crate");
}

#[test]
fn system_test_entrypoint_bin_lib_crate_lib_wins() {
    run_test("entrypoint_bin_lib_crate_lib_wins");
}

#[test]
fn system_test_option_conf_file_entrypoint_bin() {
    run_test("option_conf_file_entrypoint_bin");
}

#[test]
fn system_test_option_conf_file_entrypoint_select_bin() {
    run_test("option_conf_file_entrypoint_select_bin");
}

#[test]
fn system_test_option_conf_file_entrypoint_select_bin_custom_path() {
    run_test("option_conf_file_entrypoint_select_bin_custom_path");
}

#[test]
fn system_test_option_cmd_entrypoint_bin() {
    let test_name = "option_cmd_entrypoint_bin";

    let option = TestOptions { args: &["--entrypoint", "bin"], ..TestOptions::default() };

    run_test_with_options(test_name, option);
}

#[test]
fn system_test_option_cmd_entrypoint_select_bin() {
    let test_name = "option_cmd_entrypoint_select_bin";

    let option = TestOptions { args: &["--entrypoint", "bin:foo"], ..TestOptions::default() };

    run_test_with_options(test_name, option);
}

#[test]
fn system_test_option_cmd_entrypoint_select_bin_custom_path() {
    let test_name = "option_cmd_entrypoint_select_bin_custom_path";

    let option = TestOptions { args: &["--entrypoint", "bin:foo"], ..TestOptions::default() };

    run_test_with_options(test_name, option);
}

#[test]
fn system_test_marker_inside_doc() {
    run_test("marker_inside_doc");
}

#[test]
fn system_test_rust_code_block_remove_comments() {
    run_test("rust_code_block_remove_comments");
}

#[test]
fn system_test_avoid_overwrite_uncommited_readme() {
    use std::fs::File;

    let test_name = "avoid_overwrite_uncommited_readme";
    let readme_path = test_dir(test_name).join("README.md");

    let option = TestOptions {
        check_readme_expected: false,
        expected_exit_status: 3,
        force: false,
        ..TestOptions::default()
    };

    let mut file = File::create(&readme_path).unwrap();
    file.write_all("A file!".as_bytes()).unwrap();
    std::mem::drop(file);

    run_test_with_options(test_name, option);

    let mut file = File::open(&readme_path).unwrap();
    let mut content = String::new();
    file.read_to_string(&mut content).unwrap();

    assert_eq!(content, "A file!");
}

#[test]
fn system_test_rust_code_block_add_markdown_tag() {
    run_test("rust_code_block_add_markdown_tag");
}

#[test]
fn system_test_transform_rust_code_nested_fenced_blocks() {
    run_test("transform_rust_code_nested_fenced_blocks");
}
