use std::path::{Path, PathBuf};
use std::process::Command;

fn test_dir(test_name: &str) -> PathBuf {
    let project_dir = std::env::current_dir().unwrap();
    project_dir.join("tests").join(test_name)
}

const BIN_PATH: &'static str = env!(concat!("CARGO_BIN_EXE_", env!("CARGO_PKG_NAME")));

fn run_test(test_name: &str) {
    let bin_path = Path::new(BIN_PATH);
    let test_dir = test_dir(test_name);

    if !bin_path.is_file() {
        panic!("Binary not found: {}", bin_path.display());
    }

    if !test_dir.is_dir() {
        panic!("Test directory not found: {}", test_dir.display());
    }

    Command::new(&bin_path)
        .current_dir(test_dir)
        .output()
        .expect(&format!("Failed to execute {}", bin_path.display()));
}

#[test]
fn integration_test_simple_single_marker() {
    run_test("simple_single_marker");
}
