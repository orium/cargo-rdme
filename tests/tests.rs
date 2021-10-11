/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use std::process::Command;

struct TestOptions {
    readme_filename: String,
}

impl Default for TestOptions {
    fn default() -> Self {
        TestOptions { readme_filename: "README.md".to_owned() }
    }
}

fn run_test_with_options(test_name: &str, options: TestOptions) {
    let cargo_rdme_dir = std::env::current_dir().unwrap();

    let cargo_rdme_bin = {
        let name = std::env::var("CARGO_PKG_NAME").unwrap();

        // This ensures the tests run in windows as well.
        [&name, &format!("{}.exe", name)]
            .iter()
            .map(|filename| cargo_rdme_dir.join("target").join("debug").join(filename))
            .find(|bin| bin.is_file())
            .expect(&format!("{} binary not found", name))
    };

    let test_dir = cargo_rdme_dir.join("tests").join(test_name);

    if !test_dir.is_dir() {
        panic!("Test directory not found: {}", test_dir.display());
    }

    let expected_readme = test_dir.join("README-expected.md");

    if !expected_readme.is_file() {
        panic!("Expected readme not found: {}", expected_readme.display());
    }

    let template_readme = test_dir.join("README-template.md");

    if !template_readme.is_file() {
        panic!("Template readme not found: {}", template_readme.display());
    }

    let readme = test_dir.join(options.readme_filename);

    std::fs::copy(&template_readme, &readme).unwrap();

    let output = Command::new(&cargo_rdme_bin)
        .current_dir(test_dir)
        .output()
        .expect(&format!("Failed to execute {}", cargo_rdme_bin.display()));

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

        let stderr = String::from_utf8_lossy(&output.stderr);

        panic!(
            "The generated README does not match what was expected.\n\n{}\n==== stderr ====\n{}",
            diff_msg, stderr
        );
    } else {
        std::fs::remove_file(readme).unwrap();
    }
}

fn run_test(test_name: &str) {
    run_test_with_options(test_name, TestOptions::default())
}

#[test]
fn integration_test_simple_single_marker() {
    run_test("simple_single_marker");
}

#[test]
fn integration_test_simple_single_marker_no_footer() {
    run_test("simple_single_marker_no_footer");
}

#[test]
fn integration_test_simple_start_end_marker() {
    run_test("simple_start_end_marker");
}

#[test]
fn integration_test_custom_lib_path() {
    run_test("custom_lib_path");
}

#[test]
fn integration_test_custom_readme_path() {
    let option = TestOptions { readme_filename: "READ-ME.md".to_owned() };

    run_test_with_options("custom_readme_path", option);
}

#[test]
fn integration_test_line_terminator_crlf() {
    run_test("line_terminator_crlf");
}

#[test]
fn integration_test_multiline_doc() {
    run_test("multiline_doc");
}
