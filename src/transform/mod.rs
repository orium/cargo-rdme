use crate::Doc;

mod intralinks;
mod rust_markdown_tag;
mod rust_remove_comments;
mod utils;

pub use intralinks::{
    DocTransformIntralinks, EXPECTED_RUST_TOOLCHAIN, IntralinkError, IntralinksConfig,
    IntralinksDocsRsConfig, install_expected_rust_toolchain, is_expected_rust_toolchain_installed,
};
pub use rust_markdown_tag::DocTransformRustMarkdownTag;
pub use rust_remove_comments::DocTransformRustRemoveComments;

pub trait DocTransform {
    type E;

    fn transform(&self, doc: &Doc) -> Result<Doc, Self::E>;
}
