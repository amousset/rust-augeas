rust-augeas
===========

[![crates.io](https://img.shields.io/crates/v/augeas.svg)](https://crates.io/crates/augeas) [![Documentation](https://docs.rs/augeas/badge.svg)](https://docs.rs/augeas) [![CI](https://github.com/amousset/rust-augeas/actions/workflows/ci.yml/badge.svg)](https://github.com/amousset/rust-augeas/actions/workflows/ci.yml)

Rust binding for [Augeas](https://github.com/hercules-team/augeas).

## Design

This library is a low-level binding to the C API of Augeas, with a few abstractions to make it more idiomatic to use in Rust. It does not aim to provide a high-level API to manipulate configuration files, but rather to provide a safe and idiomatic way to interact with Augeas.

## TODO

* Consider allowing non-UTF-8 strings for paths and values.
* Consider adding missing bindings if it makes sense:
  * `aug_print`
  * `aug_to_xml`
  * `aug_srun`

