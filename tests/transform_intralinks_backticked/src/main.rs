//! This [beautiful crate](`crate`) is cool because it contains [modules](`crate::amodule`) and some
//! other [stuff](https://en.wikipedia.org/wiki/Stuff) as well.
//!
//! This link is [broken](`crate::broken`), but this should [wor\\k \[ju\]st](f\\i\(n\)e).
//!
//! Go ahead and check all the [structs in foo](`crate::foo#structs`) and
//! [structs in foo](`crate::foo`#structs) specifically [this one](`crate::foo::BestStruct`)
//!
//! [![BestStruct doc](https://example.com/image.png)](`crate::foo::BestStruct`)

pub mod amodule {}

pub mod foo {
  pub struct BestStruct {}
}

fn main() {}
