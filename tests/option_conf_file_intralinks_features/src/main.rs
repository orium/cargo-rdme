//! This crate contains [foo](crate::foo) and [bar](crate::bar).

#[cfg(feature = "foo")]
fn foo() {

}

#[cfg(feature = "bar")]
fn bar() {

}

fn main() {}
