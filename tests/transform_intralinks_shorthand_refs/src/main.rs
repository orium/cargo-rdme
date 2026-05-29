//! The shorthand pass must not corrupt reference links or reference definitions.
//!
//! Bare shorthand is converted to a link: [`BareStruct`].
//!
//! Shortcut reference with an explicit definition is left untouched: [`DefStruct`].
//!
//! Full reference link is left untouched: [`RefStruct`][rs].
//!
//! [`DefStruct`]: crate::DefStruct
//! [rs]: crate::RefStruct

struct BareStruct {}
struct DefStruct {}
struct RefStruct {}

fn main() {}
