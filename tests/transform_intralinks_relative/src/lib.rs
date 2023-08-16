//! Relative intralinks from the crate root.
//!
//! Bare names:
//!
//! * [`Foo`](Foo)
//! * [`bare_fn`](bare_fn)
//!
//! `self::` form:
//!
//! * [`self::Foo`](self::Foo)
//! * [`self::bare_fn`](self::bare_fn)
//! * [`self::amod::AStruct`](self::amod::AStruct)
//!
//! Multi-level module-relative:
//!
//! * [`amod::AStruct`](amod::AStruct)
//! * [`amod::inner::DeepStruct`](amod::inner::DeepStruct)
//! * [`amod::inner::deep_fn`](amod::inner::deep_fn)

pub struct Foo {}

pub fn bare_fn() {}

pub mod amod {
    pub struct AStruct {}

    pub mod inner {
        pub struct DeepStruct {}

        pub fn deep_fn() {}
    }
}
