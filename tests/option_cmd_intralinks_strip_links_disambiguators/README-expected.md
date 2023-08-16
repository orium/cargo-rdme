<!-- cargo-rdme start -->

Disambiguator-style intralinks should be stripped when `strip-links` is enabled:

* `type@`: Foo via type@
* `struct@`: Foo via struct@
* `fn@`: my_fn via fn@
* `macro@`: my_macro via macro@
* `mod@`: amod via mod@
* suffix `()`: my_fn via paren
* suffix `!`: my_macro via bang

<!-- cargo-rdme end -->
