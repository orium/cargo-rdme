//! Impl blocks defined in modules other than where the type lives:
//!
//! * [`Foo::a_method`](Foo::a_method)
//! * [`Foo::NUMBER`](Foo::NUMBER)
//! * [`Foo::Baz`](Foo::Baz)
//! * [`Foo::another_method`](Foo::another_method)

// TODO Remove this when `inherent_associated_types` stabilizes (https://github.com/rust-lang/rust/issues/8995).
#![feature(inherent_associated_types)]

pub struct Foo {}

mod a_mod {
    impl super::Foo {
        pub const NUMBER: usize = 42;

        pub type Baz = u32;

        pub fn a_method(&self) -> u32 {
            42
        }
    }
}

mod b_mod {
    impl super::Foo {
        pub fn another_method(&self) -> u32 {
            42
        }
    }
}
