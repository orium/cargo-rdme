//! * [struct foo::AStruct](foo::AStruct)
//! * [macro foo::foo!](foo::foo)
//! * This is ambiguous and should be striped: [mod bar](bar)
//! * [mod bar](mod@bar)
//! * [macro bar!](bar!)

#[macro_export]
macro_rules! bar {
    () => {{}};
}

pub mod bar {}

pub mod foo {
    #[macro_export]
    macro_rules! foo {
        () => {{}};
    }

    pub use foo;

    pub struct AStruct {}
}
