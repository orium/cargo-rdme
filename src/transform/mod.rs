/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

use crate::Doc;

pub mod rust_remove_comments;

pub trait DocTransform {
    type E;

    fn transform(&self, doc: &Doc) -> Result<Doc, Self::E>;
}

mod utils;
