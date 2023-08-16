//! Re-exported module (mirroring the example from issue #5):
//!
//! [`crate::bar::Bar`](crate::bar::Bar)

mod foo {
    pub mod bar {
        pub struct Bar {}
    }
}

pub use foo::bar;
