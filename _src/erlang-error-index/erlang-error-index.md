---
sidebar_position: 5
---

# Erlang Error Index

The _Erlang Error Index_ is a website, inspired by the [Rust Error Index](https://doc.rust-lang.org/error_codes/error-index.html) and the [Haskell Error Index](https://discourse.haskell.org/t/announcing-the-haskell-error-index/5195).

In ELP, each _diagnostic_ (error or warning) is associated to a unique code. This code can be looked up on the index to find explanations and examples.

The Erlang Error Index is powered by _Markdown_ files, so you don’t need to be an expert to contribute. All sort of contributions to the Erlang Error Index are extremely welcome.

## Namespaces

Error codes are grouped by using _namespaces_. Each namespace is associated to the tool that emits those error codes.

Are we missing a tool? Reserve a namespace by [making a Pull Request](https://github.com/WhatsApp/erlang-language-platform/tree/main/website/docs)!

| Tool | Namespace |
|-|-|
|[Erlang Compiler](./c/about)|`C`|
|[Erlang EPP Dodger](./d/about)|`D`|
|[Erlang Pre-processor](./e/about)|`E`|
|[Erlang Linter](./l/about)|`L`|
|[Erlang EDoc](./o/about)|`O`|
|[Erlang Parser](./p/about)|`P`|
|[Erlang Scanner](./s/about)|`S`|
|[WhatsApp ELP](./w/about)|`W`|

## Ignoring Diagnostics

ELP provides a generic mechanism to ignore instances of an error code.

Given the error code `X12345` you can ignore a diagnostic by prepending the offending line with a special annotation:

```
% elp:ignore X12345
```

You can also ignore a diagnostic by *alias*. E.g.:

```
% elp:ignore atoms_exhaustion
```

To find the human readable alias for a given code you can use the `elp explain` command:

```
$ elp explain --code W0023
https://whatsapp.github.io/erlang-language-platform/docs/erlang-error-index/w/W0023/ (atoms_exhaustion)
```

It is possible to ignore multiple error codes at once:

```
% elp:ignore X12345 Y56789
```
