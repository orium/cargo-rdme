use crate::Doc;

mod intralinks;
mod rust_markdown_tag;
mod rust_remove_comments;
mod utils;

pub use intralinks::{
    DocTransformIntralinks, IntralinkError, IntralinksConfig, IntralinksDocsRsConfig,
};
pub use rust_markdown_tag::DocTransformRustMarkdownTag;
pub use rust_remove_comments::DocTransformRustRemoveComments;

pub trait DocTransform {
    type E;

    fn transform(&self, doc: &Doc) -> Result<Doc, Self::E>;
}
