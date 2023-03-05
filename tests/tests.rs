/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

mod testing;

use crate::testing::{test_dir, test_readme_expected, test_readme_template};
use cargo_rdme::{infer_line_terminator, LineTerminator};
use std::io::{Read, Write};
use testing::{run_test, run_test_with_options, TestOptions};

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
    let option = TestOptions { readme_filename: "READ-ME.md", ..TestOptions::default() };

    run_test_with_options("custom_readme_path", option);
}

#[test]
fn integration_test_line_terminator_crlf() {
    let test_name = "line_terminator_crlf";
    let readme_template = test_readme_template(test_name);
    let readme_expected = test_readme_expected(test_name);

    assert_eq!(infer_line_terminator(readme_template).unwrap(), LineTerminator::CrLf);
    assert_eq!(infer_line_terminator(readme_expected).unwrap(), LineTerminator::CrLf);

    run_test(test_name);
}

#[test]
fn integration_test_multiline_doc() {
    run_test("multiline_doc");
}

#[test]
fn integration_test_option_cmd_override_readme_path() {
    let test_name = "option_cmd_override_readme_path";

    let option = TestOptions {
        args: &["--readme-path", "r.md"],
        readme_filename: "r.md",
        ..TestOptions::default()
    };

    run_test_with_options(test_name, option);
}

#[test]
fn integration_test_option_cmd_line_terminator_lf() {
    let test_name = "option_cmd_line_terminator_lf";
    let readme_template = test_readme_template(test_name);
    let readme_expected = test_readme_expected(test_name);

    assert_eq!(infer_line_terminator(readme_template).unwrap(), LineTerminator::CrLf);
    assert_eq!(infer_line_terminator(readme_expected).unwrap(), LineTerminator::Lf);

    let option = TestOptions { args: &["--line-terminator", "lf"], ..TestOptions::default() };

    run_test_with_options(test_name, option);
}

#[test]
fn integration_test_option_cmd_line_terminator_crlf() {
    let test_name = "option_cmd_line_terminator_crlf";
    let readme_template = test_readme_template(test_name);
    let readme_expected = test_readme_expected(test_name);

    assert_eq!(infer_line_terminator(readme_template).unwrap(), LineTerminator::Lf);
    assert_eq!(infer_line_terminator(readme_expected).unwrap(), LineTerminator::CrLf);

    let option = TestOptions { args: &["--line-terminator", "crlf"], ..TestOptions::default() };

    run_test_with_options(test_name, option);
}

#[test]
fn integration_test_option_conf_file_override_readme_path() {
    let test_name = "option_conf_file_override_readme_path";

    let option = TestOptions { readme_filename: "r.md", ..TestOptions::default() };

    run_test_with_options(test_name, option);
}

#[test]
fn integration_test_option_conf_file_line_terminator_lf() {
    let test_name = "option_conf_file_line_terminator_lf";
    let readme_template = test_readme_template(test_name);
    let readme_expected = test_readme_expected(test_name);

    assert_eq!(infer_line_terminator(readme_template).unwrap(), LineTerminator::CrLf);
    assert_eq!(infer_line_terminator(readme_expected).unwrap(), LineTerminator::Lf);

    run_test(test_name);
}

#[test]
fn integration_test_option_conf_file_line_terminator_crlf() {
    let test_name = "option_conf_file_line_terminator_crlf";
    let readme_template = test_readme_template(test_name);
    let readme_expected = test_readme_expected(test_name);

    assert_eq!(infer_line_terminator(readme_template).unwrap(), LineTerminator::Lf);
    assert_eq!(infer_line_terminator(readme_expected).unwrap(), LineTerminator::CrLf);

    run_test(test_name);
}

#[test]
fn integration_test_option_cmd_check_ok() {
    let test_name = "option_cmd_check_ok";
    let option = TestOptions {
        args: &["--check"],
        check_readme_expected: false,
        expected_exit_code: 0,
        ..TestOptions::default()
    };

    run_test_with_options(test_name, option);
}

#[test]
fn integration_test_option_cmd_check_fail() {
    let test_name = "option_cmd_check_fail";
    let option = TestOptions {
        args: &["--check"],
        check_readme_expected: false,
        expected_exit_code: 3,
        ..TestOptions::default()
    };

    run_test_with_options(test_name, option);
}

#[test]
fn integration_test_option_cmd_check_fail_because_warnings() {
    let test_name = "option_cmd_check_fail_because_warnings";
    let option = TestOptions {
        args: &["--check"],
        check_readme_expected: false,
        expected_exit_code: 4,
        ..TestOptions::default()
    };

    run_test_with_options(test_name, option);
}

#[test]
fn integration_test_option_cmd_check_no_fail_on_warnings() {
    let test_name = "option_cmd_check_no_fail_on_warnings";
    let option = TestOptions {
        args: &["--check", "--no-fail-on-warnings"],
        check_readme_expected: false,
        expected_exit_code: 0,
        ..TestOptions::default()
    };

    run_test_with_options(test_name, option);
}

#[test]
fn integration_test_option_cmd_check_fail_line_terminator() {
    let test_name = "option_cmd_check_fail_line_terminator";

    // First check that the test would pass without the line terminator override.
    let option = TestOptions {
        args: &["--check"],
        check_readme_expected: false,
        expected_exit_code: 0,
        ..TestOptions::default()
    };

    run_test_with_options(test_name, option);

    let option = TestOptions {
        args: &["--check", "--line-terminator", "crlf"],
        check_readme_expected: false,
        expected_exit_code: 3,
        ..TestOptions::default()
    };

    run_test_with_options(test_name, option);
}

#[test]
fn integration_test_entrypoint_bin_crate() {
    run_test("entrypoint_bin_crate");
}

#[test]
fn integration_test_entrypoint_bin_lib_crate_lib_wins() {
    run_test("entrypoint_bin_lib_crate_lib_wins");
}

#[test]
fn integration_test_option_conf_file_entrypoint_bin() {
    run_test("option_conf_file_entrypoint_bin");
}

#[test]
fn integration_test_option_conf_file_entrypoint_select_bin() {
    run_test("option_conf_file_entrypoint_select_bin");
}

#[test]
fn integration_test_option_conf_file_entrypoint_select_bin_custom_path() {
    run_test("option_conf_file_entrypoint_select_bin_custom_path");
}

#[test]
fn integration_test_option_cmd_entrypoint_bin() {
    let test_name = "option_cmd_entrypoint_bin";

    let option = TestOptions { args: &["--entrypoint", "bin"], ..TestOptions::default() };

    run_test_with_options(test_name, option);
}

#[test]
fn integration_test_option_cmd_entrypoint_select_bin_single() {
    let test_name = "option_cmd_entrypoint_select_bin_single";
    let option = TestOptions { args: &["--entrypoint", "bin"], ..TestOptions::default() };

    run_test_with_options(test_name, option);
}

#[test]
fn integration_test_option_cmd_entrypoint_select_bin() {
    let test_name = "option_cmd_entrypoint_select_bin";

    let option = TestOptions { args: &["--entrypoint", "bin:foo"], ..TestOptions::default() };

    run_test_with_options(test_name, option);
}

#[test]
fn integration_test_option_cmd_entrypoint_select_bin_custom_path() {
    let test_name = "option_cmd_entrypoint_select_bin_custom_path";

    let option = TestOptions { args: &["--entrypoint", "bin:foo"], ..TestOptions::default() };

    run_test_with_options(test_name, option);
}

#[test]
fn integration_test_separate_bin_and_lib() {
    let test_name = "separate_bin_and_lib";

    let option = TestOptions { args: &["--entrypoint", "bin"], ..TestOptions::default() };

    run_test_with_options(test_name, option);
}

#[test]
fn integration_test_marker_inside_doc() {
    run_test("marker_inside_doc");
}

#[test]
fn integration_test_avoid_overwrite_uncommitted_readme() {
    use std::fs::File;

    let test_name = "avoid_overwrite_uncommitted_readme";
    let readme_path = test_dir(test_name).join("README.md");

    let option = TestOptions {
        check_readme_expected: false,
        expected_exit_code: 2,
        force: false,
        ..TestOptions::default()
    };

    let mut file = File::create(&readme_path).unwrap();
    file.write_all("A file!".as_bytes()).unwrap();
    drop(file);

    run_test_with_options(test_name, option);

    let mut file = File::open(&readme_path).unwrap();
    let mut content = String::new();
    file.read_to_string(&mut content).unwrap();

    assert_eq!(content, "A file!");
}

#[test]
fn integration_test_transform_rust_code_block_remove_comments() {
    run_test("transform_rust_code_block_remove_comments");
}

#[test]
fn integration_test_transform_rust_code_block_add_markdown_tag() {
    run_test("transform_rust_code_block_add_markdown_tag");
}

#[test]
fn integration_test_transform_rust_code_nested_fenced_blocks() {
    run_test("transform_rust_code_nested_fenced_blocks");
}

#[test]
fn integration_test_transform_intralinks_simple() {
    run_test("transform_intralinks_simple");
}

#[test]
fn integration_test_transform_intralinks_method() {
    run_test("transform_intralinks_impl_items");
}

#[test]
fn integration_test_transform_intralinks_reference_links() {
    run_test("transform_intralinks_reference_links");
}

#[test]
fn integration_test_transform_intralinks_module_walk() {
    run_test("transform_intralinks_module_walk");
}

#[test]
fn integration_test_transform_intralinks_ambiguous_module() {
    run_test("transform_intralinks_ambiguous_module");
}

#[test]
fn integration_test_transform_intralinks_stdlib_links() {
    run_test("transform_intralinks_stdlib_links");
}

#[test]
fn integration_test_transform_intralinks_crate_name_hyphen() {
    run_test("transform_intralinks_crate_name_hyphen");
}

#[test]
fn integration_test_transform_intralinks_backticked() {
    run_test("transform_intralinks_backticked");
}

#[test]
fn integration_test_option_conf_file_workspace() {
    run_test("option_conf_file_workspace");
}

#[test]
fn integration_test_option_cmd_workspace() {
    let test_name = "option_cmd_workspace";

    let option = TestOptions {
        args: &["--workspace-project", "otherproj", "--readme-path", "README.md"],
        ..TestOptions::default()
    };

    run_test_with_options(test_name, option);
}

#[test]
fn integration_test_option_conf_file_intralinks_docs_rs_base_url() {
    run_test("option_conf_file_intralinks_docs_rs_base_url");
}

#[test]
fn integration_test_option_conf_file_intralinks_docs_rs_version() {
    run_test("option_conf_file_intralinks_docs_rs_version");
}

#[test]
fn integration_test_option_conf_file_intralinks_strip_links() {
    run_test("option_conf_file_intralinks_strip_links");
}

#[test]
fn integration_test_option_cmd_intralinks_strip_links() {
    let test_name = "option_cmd_intralinks_strip_links";

    let option = TestOptions { args: &["--intralinks-strip-links"], ..TestOptions::default() };

    run_test_with_options(test_name, option);
}

#[test]
fn integration_test_heading_level_auto_bump() {
    run_test("heading_level_auto_bump");
}

#[test]
fn integration_test_option_conf_file_heading_base_level() {
    run_test("option_conf_file_heading_base_level");
}

#[test]
fn integration_test_option_cmd_heading_base_level() {
    let test_name = "option_cmd_heading_base_level";

    let option = TestOptions { args: &["--heading-base-level", "3"], ..TestOptions::default() };

    run_test_with_options(test_name, option);
}

#[test]
fn integration_test_crate_procmacro() {
    run_test("crate_procmacro");
}
