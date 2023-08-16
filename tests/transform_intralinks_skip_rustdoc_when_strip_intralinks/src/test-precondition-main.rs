//! Crate with an [`main`](main) to prove that rustdoc fails with this. Otherwise the test is not
//! testing that rustdoc is not running.

#![deny(rustdoc::broken_intra_doc_links)]

mod no_module_here;

fn main() {

}
