//! Shortcut intralinks that do not resolve should be preserved unchanged (no warning, no strip):
//!
//! * [`Foo`]
//! * [`NonExistent`]
//! * [missing_module]
//! * [absent::Item]

pub struct Foo {}
