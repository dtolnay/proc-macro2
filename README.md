# proc-macro2

[Documentation](http://alexcrichton.com/proc-macro2)

A small shim over the `proc_macro` crate intended to multiplex the current
stable interface (as of 2017-05-19) and the [upcoming richer
interface][upcoming].

[upcoming]: https://github.com/rust-lang/rust/pull/40939

The upcoming support has features like:

* Span information on tokens
* No need to go in/out through strings
* Structured input/output

My hope is that libraries ported to `proc_macro2` will be trivial to port to the
real `proc_macro` crate once the support on nightly is stabilize.

This crate is still very much a work in progress

# License

`proc-macro2` is primarily distributed under the terms of both the MIT license and
the Apache License (Version 2.0), with portions covered by various BSD-like
licenses.

See LICENSE-APACHE, and LICENSE-MIT for details.
