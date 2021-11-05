/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use crate::Doc;

mod intralinks;
mod rust_markdown_tag;
mod rust_remove_comments;
mod utils;

pub use intralinks::{DocTransformIntralinks, IntralinkError};
pub use rust_markdown_tag::DocTransformRustMarkdownTag;
pub use rust_remove_comments::DocTransformRustRemoveComments;

pub trait DocTransform {
    type E;

    fn transform(&self, doc: &Doc) -> Result<Doc, Self::E>;
}
