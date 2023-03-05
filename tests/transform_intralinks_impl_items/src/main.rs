//! This crate has [`Foo::new()`](`crate::Foo::new`), [`Foo::a_method()`](`crate::Foo::a_method`),
//! and [`Foo::another_method()`](`crate::Foo::another_method`).
//!
//! It also has [`Foo::no_self()`](`crate::Foo::no_self`).  There's also [`Bar::beer()`](`crate::amod::Bar::beer`).
//!
//! Struct `Foo` has a [type called `baz`](`crate::Foo::Baz`) and a
//! [const called `number`](`crate::Foo::number`).
//!
//! We have a function in `FooAlias` [called `hello`](`crate::FooAlias::hello`).
//!
//! And in `MyEnum` we have [called `hey`](`crate::MyEnum::hey`).
//!
//! And in `MyUnion` we have [called `sup`](`crate::MyUnion::sup`).

// TODO Remove this when inherent_associated_types stabilizes.
#![feature(inherent_associated_types)]

pub struct Foo {}

impl Foo {
    const number: usize = 1234;

    type Baz = u32;

    pub fn new() -> Foo {
        Foo {}
    }

    pub fn a_method(&self) -> u32 {
        42
    }

    pub fn no_self(s: &str) -> usize {
        s.len()
    }
}

impl Foo {
    pub fn another_method(&self, x: u32) -> u32 {
        self.a_method() + x
    }
}

mod amod {
    pub struct Bar {}

    impl Bar {
        pub fn beer(&self) {}
    }
}

type FooAlias = Foo;

impl FooAlias {
    fn hello(&self) {}
}

enum MyEnum {}

impl MyEnum {
    fn hey(&self) {}
}

union MyUnion {}

impl MyUnion {
    fn sup(&self) {}
}

fn main() {}
