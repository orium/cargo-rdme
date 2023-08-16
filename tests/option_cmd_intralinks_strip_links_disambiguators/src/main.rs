//! Disambiguator-style intralinks should be stripped when `strip-links` is enabled:
//!
//! * `type@`: [Foo via type@](type@crate::Foo)
//! * `struct@`: [Foo via struct@](struct@crate::Foo)
//! * `fn@`: [my_fn via fn@](fn@crate::my_fn)
//! * `macro@`: [my_macro via macro@](macro@crate::my_macro)
//! * `mod@`: [amod via mod@](mod@crate::amod)
//! * suffix `()`: [my_fn via paren](crate::my_fn())
//! * suffix `!`: [my_macro via bang](crate::my_macro!)

pub struct Foo {}

pub fn my_fn() {}

#[macro_export]
macro_rules! my_macro {
    () => {{}};
}

pub mod amod {}

fn main() {}
