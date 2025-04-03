---
sidebar_position: 3
---

# Contributing

## To the Language Server

To contribute to the ELP language server, please refer to [these instructions](https://github.com/WhatsApp/erlang-language-platform/blob/main/CONTRIBUTING.md).

## To the website

The website is powered by [Docusaurus](https://docusaurus.io/). Most of the content is powered by [Markdown](https://docusaurus.io/docs/markdown-features).

You can find the source code under the [website](https://github.com/WhatsApp/erlang-language-platform/tree/main/website) folder in the ELP repository. More detailed instructions on how to run the website locally are available [here](https://github.com/WhatsApp/erlang-language-platform/tree/main/website#readme), but the short story is:

```bash
git clone https://github.com/WhatsApp/erlang-language-platform.git
cd erlang-language-platform/website
yarn install
yarn build
yarn start
```

The above will start a local server. You can start contributing content while watching live changes at:

```
http://localhost:3000
```

## To the Erlang Error Index

The Erlang Error Index is currently part of the ELP website, so you can refer to the instructions [above](#to-the-website) on how to contribute content.

The entrypoint for the index is [here](https://github.com/WhatsApp/erlang-language-platform/tree/main/website/docs/erlang-error-index).

Error codes are organized using *namespaces*, which are listed [here](https://github.com/WhatsApp/erlang-language-platform/blob/main/website/docs/erlang-error-index/erlang-error-index.mdx#namespaces).
Generally speaking, a namespace corresponds to a tool which emits error codes (e.g. `C` for the `Erlang Compiler`).
A folder is associated to each namespace. If the namespace is `C`, the corresponding folder is named `c` (lowered version of the namespace).

Under each folder you will see the following files:

* `_category_.json`: Metadata for the namespace, such as a label and the relative position in the list of namespaces
* `about.md`: High level introduction to the namespace (e.g. which tool is emitting codes for the given namespace)
* A number of `X1234.md` files: A file for each error code emitted

You can find the list of emitted error codes [here](https://github.com/WhatsApp/erlang-language-platform/blob/29794ce37296222e2ce294499eddab6b8c9a32b1/erlang_service/src/erlang_service.erl#L603).

High-quality content contributions to the Error Index are extremely welcome!
