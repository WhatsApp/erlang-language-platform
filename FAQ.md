# Frequently Asked Questions

## What's the difference between ELP and Erlang LS?

Erlang LS is a language server for the Erlang programming language, connecting OTP and third-party tools - such as the OTP Compiler, EDoc, XRef, EqWAlizer, Wrangler, etc - to the IDE via the LSP protocol.
ELP is a compiler front-end for Erlang. It provides a scalable, fully incremental, IDE-first library for the semantic analysis of Erlang code, easing the implementation for code-related tools — linters, static analysers, type checkers, formatters, refactoring tools, smart code generation, and more.
By implementing support for the LSP protocol, ELP also acts as a language server, effectively superseding Erlang LS.
ELP is to the Erlang compiler what [Rust Analyzer](https://rust-analyzer.github.io/) is to the main Rust compiler.

## Why not extend Erlang LS, rather than creating a new tool?

Erlang LS acts as the "glue" between existing OTP tools (Compiler, EDoc, XRef, etc) and the IDE. These tools suffer a fundamental problem: they only work with valid code and they are often not incremental, requiring a new analysis every time they are invoked. This is often the opposite of what is needed in the highly interactive environment which the IDE is. Most of the time the IDE has to work with invalid code (the code you are typing) and it has to work with it repeatedly, after applying small, localised changes. Providing a solution for this requires a fundamentally different architecture and approach.

## Why is ELP implemented in Rust, rather than Erlang?

There are a few reasons for our choice:

* ELP required two big components to exist: a parser generator with good error recovery and a framework for incremental computation. Both compoonents (respectively [Tree Sitter](https://tree-sitter.github.io/tree-sitter/) and [Salsa](https://github.com/salsa-rs/salsa) are available for Rust but not yet for Erlang
* The LSP libraries for Erlang are only partially implemented (as part of Erlang LS), while they are complete for Rust
* ELP is modeled after Rust Analyzer, which is itself implemented in Rust. Having a widely used reference was extremely helpful during development
