<!-- cargo-rdme start -->

Link targets containing `/` are never intralinks and must be preserved unchanged:

* [absolute path](/foo/bar)
* [relative path](./baz)
* [parent path](../qux)
* [bare path with slash](dir/file)
* [url](https://example.com/path)

A real intralink alongside them is still rewritten:

* [`Foo`](https://docs.rs/integration_test/latest/integration_test/struct.Foo.html)

<!-- cargo-rdme end -->
