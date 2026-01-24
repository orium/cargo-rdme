//! This crate contains no [intralinks](https://en.wikipedia.org/wiki/Stuff) so rustdoc should not
//! run. We test that by having a broken rust file that rustdoc would not [accept].
//!
//! [accept]: https://en.wikipedia.org/wiki/Stuff

#![deny(rustdoc::broken_intra_doc_links)]

mod no_module_here;

fn main() {

}
