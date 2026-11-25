---
sidebar_position: 3
---

# Contributing

## To the Language Server

To contribute to the ELP language server, please refer to [these instructions](https://github.com/WhatsApp/erlang-language-platform/blob/main/CONTRIBUTING.md).

## To the VS Code extension

The extension lives under [editors/code](https://github.com/WhatsApp/erlang-language-platform/tree/main/editors/code).

### Try a build from CI

Every CI run packages the extension, with ELP and its tools bundled, for each platform and OTP release. To try the extension of a pull request:

1. Open the run from the *Checks* tab of the pull request and download the artifact for your platform and OTP release from its *Artifacts* section, e.g. `elp-macos-aarch64-apple-darwin-otp-28.5.vsix`. The artifact is a zip file that contains `erlang-language-platform.vsix`.
2. Install it in a VS Code profile of its own, so that it doesn't replace the extension you use day to day:

```bash
unzip elp-macos-aarch64-apple-darwin-otp-28.5.vsix.zip
code --profile elp-test --install-extension erlang-language-platform.vsix --force
```

The `code` command is available after running *Shell Command: Install 'code' command in PATH* from the VS Code command palette.

3. Open a project in that profile, with Erlang/OTP, `rebar3` and, for Buck2 projects, [Buck2](https://buck2.build/docs/getting_started/) on the `PATH`. Quit VS Code first, so that it picks up the `PATH` of your shell:

```bash
code --profile elp-test path/to/your/project
```

The [test projects](https://github.com/WhatsApp/erlang-language-platform/tree/main/test/test_projects) of the repository are a good place to start: `end_to_end` is a Buck2 project with examples for most features, and `codegen_test` a Buck2 project with generated files.

A CI build has the version in `editors/code/package.json`, which a release from the marketplace can share, so the *Extensions* view can't tell the two apart: install it in a profile of its own, as above.

### Run the end-to-end tests

The end-to-end tests start VS Code with the extension and check its features on the test projects. They need the same tools as the extension, on the `PATH`: Erlang/OTP, `rebar3` and Buck2. Debugging tests also need OTP 28 or later, and are skipped on earlier releases.

Build ELP [from source](../get-started/install.md#from-source) and put the binaries the extension bundles in `editors/code/bin`, as the *Package ... into VS Code Extension* steps of [ci.yml](https://github.com/WhatsApp/erlang-language-platform/blob/main/.github/workflows/ci.yml) do: at least `elp`, and `edb` from an [EDB release](https://github.com/WhatsApp/edb/releases) for the debugging tests. Then:

```bash
cd editors/code
npm install
npm test
```

`npm test` compiles the extension, downloads VS Code and runs the tests in three workspaces: `end_to_end` as a Buck2 project (`--label buck2`), `codegen_test` (`--label buck2-codegen`) and a copy of `end_to_end` as a rebar3 project (`--label rebar3`). To run only one of them, or only some tests:

```bash
npx vscode-test --label buck2 --grep 'Run in REPL'
```

On Linux without a display, run the tests under `xvfb-run -a`, as CI does.

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
