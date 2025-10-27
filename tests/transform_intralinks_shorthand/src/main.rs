//! # Shorthand Link Tests
//!
//! Basic shorthand links: [`my_function()`], [`MyStruct`], and [`MY_CONST`].
//!
//! Shorthand links in a list:
//! - [`first_function()`]
//! - [`second_function()`]
//!
//! Edge case: shorthand link followed by space and parenthesis (the bug we fixed):
//! [`important_function()`] (this is very fast).
//!
//! Multiple on one line: [`foo()`] and [`bar()`] work together.
//!
//! Already converted (should not double-convert): [`already_linked()`](https://example.com).
//!
//! Not a shorthand link (no backticks): [regular link](https://example.com).
//!
//! Nested in text: The [`nested_func()`] is useful for processing.

fn my_function() {}
fn first_function() {}
fn second_function() {}
fn important_function() {}
fn foo() {}
fn bar() {}
fn already_linked() {}
fn nested_func() {}

struct MyStruct {}

const MY_CONST: i32 = 42;

fn main() {}
