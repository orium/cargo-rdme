//! All item kinds:
//!
//! * [std](std)
//! * [Arc](std::sync::Arc)
//! * [core::cmp::Eq](core::cmp::Eq)
//! * [Path](Path)
//! * [u32](u32)
//! * [crate](crate)
//! * [amodule](amodule)
//! * [foo::BestStruct](foo::BestStruct)
//! * [MyUnion](MyUnion)
//! * [MyUnion::f1](MyUnion::f1)
//! * [MyEnum](MyEnum)
//! * [my_fn](my_fn)
//! * [my_fn()](my_fn())
//! * [foobini::MyType](foobini::MyType)
//! * [foobini::MyOpaqueType](foobini::MyOpaqueType)
//! * [MY_CONST](MY_CONST)
//! * [MyTrait](MyTrait)
//! * [MyTrait::T](MyTrait::T)
//! * [MyTrait::trait_method](MyTrait::trait_method)
//! * [MyTraitAlias](MyTraitAlias)
//! * [MY_STATIC](MY_STATIC)
//! * [my_macro](my_macro)
//! * [bar::BestStruct](bar::BestStruct)
//! * [bar::BestStruct::my_field](bar::BestStruct::my_field)
//! * [bar::BestStruct::0](bar::TupleStruct::0)
//! * [MyEnum::Foo](MyEnum::Foo)
//! * [StructWithImpl::NUMBER](StructWithImpl::NUMBER)
//! * [StructWithImpl::Baz](StructWithImpl::Baz)
//! * [StructWithImpl::a_method](StructWithImpl::a_method)
//! * [Foobini](Foobini)

// TODO Remove this when `type_alias_impl_trait` is stable (https://github.com/rust-lang/rust/issues/63063).
#![feature(type_alias_impl_trait)]
// TODO Remove this when `trait_alias` is stable (https://github.com/rust-lang/rust/issues/41517).
#![feature(trait_alias)]
// TODO Remove this when inherent_associated_types stabilizes (https://github.com/rust-lang/rust/issues/8995).
//      This is what allows the `type` definition in `impl StructWithImpl`.
#![feature(inherent_associated_types)]

extern crate core;

pub use std::path::Path;

#[macro_export]
macro_rules! my_macro {
    () => {{}};
}

pub const MY_CONST: usize = 32;

pub static MY_STATIC: usize = 32;

pub mod amodule {}

pub mod foo {
    pub struct BestStruct {
        pub my_field: u64,
    }
}

pub mod bar {
    pub struct BestStruct {
        pub my_field: u64,
    }

    pub struct TupleStruct(pub u64);
}

pub union MyUnion {
    f1: u32,
}

pub enum MyEnum {
    Foo,
}

pub fn my_fn() {}

pub mod foobini {
    pub type MyType = usize;
    pub type MyOpaqueType = impl Iterator<Item = ()>;
}

pub trait MyTrait {
    type T;

    fn trait_method();
}

pub trait MyTraitAlias = std::fmt::Debug + Send;

pub struct StructWithImpl {}

impl StructWithImpl {
    const NUMBER: usize = 1234;

    type Baz = u32;

    pub fn a_method(&self) -> u32 {
        42
    }
}

pub use re::Foobini;

mod re {
    pub struct Foobini {}
}