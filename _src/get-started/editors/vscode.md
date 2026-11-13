---
sidebar_position: 3
---

# VS Code

An ELP extension is [available via the VS Code marketplace](https://marketplace.visualstudio.com/items?itemName=erlang-language-platform.erlang-language-platform).

## Formatting

The extension formats Erlang files (`Format Document` and `Format Selection`)
using [erlfmt](https://github.com/WhatsApp/erlfmt). The extension bundles
erlfmt, which runs with the `escript` from your Erlang installation: the one on
the `PATH`, or the one in the directory set by `elpClient.erlangInstallationPath`
(also used by the language server, the debugger and tests). Use the
`elpClient.erlfmt.path` setting to point to a different erlfmt executable and
`elpClient.erlfmt.args` to pass extra options, such as `--print-width=120`.
