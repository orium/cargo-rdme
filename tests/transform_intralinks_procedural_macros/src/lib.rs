//! Procedural macro item kinds:
//!
//! * [DeriveFn](DeriveFn)
//! * [an_attribute](macro@an_attribute)

extern crate proc_macro;

#[proc_macro_derive(DeriveFn)]
pub fn derive_fn(_item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    "fn foo() -> u32 { 42 }".parse().unwrap()
}

#[proc_macro_attribute]
pub fn an_attribute(_attr: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    item
}
