//! Shortcut link form (rustdoc's `` [`foo::Foo`] `` syntax):
//!
//! * [`Foo`]
//! * [`amod::Bar`]
//! * [`my_fn`]
//! * [Foo]
//! * [amod::Bar]
//! * [my_fn]

pub struct Foo {}

pub mod amod {
    pub struct Bar {}
}

pub fn my_fn() {}
