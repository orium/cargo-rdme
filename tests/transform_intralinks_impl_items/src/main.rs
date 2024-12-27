//! This crate has [`Foo::new()`](`crate::Foo::new`), [`Foo::a_method()`](`crate::Foo::a_method`),
//! and [`Foo::another_method()`](`crate::Foo::another_method`).
//!
//! It also has [`Foo::no_self()`](`crate::Foo::no_self`).  There's also [`Bar::beer()`](`crate::amod::Bar::beer`)
//! in [`Bar`](`crate::amod::Bar`).
//!
//! Struct `Foo` has a [type called `baz`](`crate::Foo::Baz`) and a
//! [const called `NUMBER`](`crate::Foo::NUMBER`).
//!
//! We have a function in [`FooAlias`](`crate::FooAlias`) [called `hello`](`crate::FooAlias::hello`).
//!
//! And in `MyEnum` we have a method [called `hey`](`crate::MyEnum::hey`).
//!
//! And in `MyUnion` we have a method [called `sup`](`crate::MyUnion::sup`).

// TODO Remove this when inherent_associated_types stabilizes (https://github.com/rust-lang/rust/issues/8995).
//      This is what allows the `type` definition in `impl Foo`.
#![feature(inherent_associated_types)]

pub struct Foo {}

impl Foo {
    const NUMBER: usize = 1234;

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

union MyUnion {
    x: u32,
}

impl MyUnion {
    fn sup(&self) {}
}

fn main() {}
