//! This [beautiful crate] is cool because it contains [modules]
//! and some other [stuff] as well.
//!
//! This link is [broken] and this is [not supported],
//! but this should [wor\\k \[fi\]ne].
//!
//! Go ahead and check all the [structs in foo] specifically
//! [this one].  Also, this is a nice function: [copy][cp].
//!
//! [![BestStruct doc]][BestStruct]
//!
//! [beautiful crate]: crate
//! [modules]: crate::amodule
//! [stuff]: https://en.wikipedia.org/wiki/Stuff
//! [broken]: crate::broken
//! [not supported]: ::foo::bar
//! [wor\\k \[fi\]ne]: f\\i\(n\)e
//! [structs in foo]: crate::foo#structs
//! [this one]: crate::foo::BestStruct
//! [cp]: ::std::fs::copy#examples "A title here"
//! [BestStruct doc]: https://example.com/image.png
//! [BestStruct]: crate::foo::BestStruct

pub mod amodule {}

pub mod foo {
  pub struct BestStruct {}
}

fn main() {}
