// WIP! add !
// All item kinds:
//
// * [std](std)
// * [std::sync::Arc](std::sync::Arc)
// * [u32](u32)
// * [crate](crate)
// * [amodule](amodule)
// * [foo::BestStruct](foo::BestStruct)
// * [MyUnion](MyUnion)
// * [MyEnum](MyEnum)
// * [my_fn](my_fn)
// * [foobini::MyType](foobini::MyType)
// * [MyConst](MyConst)
// * [MyTrait](MyTrait)
// * [MyConst](MyStatic)
// * [my_macro](my_macro)
//! * [bar::BestStruct::my_field](bar::BestStruct::my_field)

#[macro_export]
macro_rules! my_macro {
    () => {{}};
}

pub const MyConst: usize = 32;

pub static MyStatic: usize = 32;

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
}

pub union MyUnion {
  f1: u32,
}

pub enum MyEnum {
  Foo
}

pub fn my_fn() {}

pub mod foobini {
  pub type MyType = usize;
}

pub trait MyTrait {}
