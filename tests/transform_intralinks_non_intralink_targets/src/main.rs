//! Link targets containing `/` are never intralinks and must be preserved unchanged:
//!
//! * [absolute path](/foo/bar)
//! * [relative path](./baz)
//! * [parent path](../qux)
//! * [bare path with slash](dir/file)
//! * [url](https://example.com/path)
//!
//! A real intralink alongside them is still rewritten:
//!
//! * [`Foo`](crate::Foo)

pub struct Foo {}

fn main() {}
