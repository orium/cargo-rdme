//! Explicit kind disambiguators:
//!
//! * [`struct@Foo`](struct@Foo)
//! * [`enum@MyEnum`](enum@MyEnum)
//! * [`trait@MyTrait`](trait@MyTrait)
//! * [`union@MyUnion`](union@MyUnion)
//! * [`mod@a_mod`](mod@a_mod)
//! * [`const@MY_CONST`](const@MY_CONST)
//! * [`static@MY_STATIC`](static@MY_STATIC)
//! * [`fn@my_fn`](fn@my_fn)
//! * [`type@MyType`](type@MyType)
//! * [`value@my_fn`](value@my_fn)
//! * [`macro@my_macro`](macro@my_macro)
//! * [`primitive@u32`](primitive@u32)

pub struct Foo {}

pub enum MyEnum {}

pub trait MyTrait {}

pub union MyUnion {
    pub f: u32,
}

pub mod a_mod {}

pub const MY_CONST: usize = 42;

pub static MY_STATIC: usize = 42;

pub fn my_fn() {}

pub type MyType = u32;

#[macro_export]
macro_rules! my_macro {
    () => {{}};
}
