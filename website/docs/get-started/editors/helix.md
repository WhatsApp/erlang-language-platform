---
sidebar_position: 6
---

# Helix

![screenshot](/img/helix.png)

The ELP project can be used as a [language server](https://microsoft.github.io/language-server-protocol/overviews/lsp/overview/)
in Helix with it's native LSP support.

To configure the LSP server, navigate to a [language configuration file](https://docs.helix-editor.com/languages.html)
(i.e. `~/.config/helix/languages.toml`) and add it's configuration to the Erlang language.

```
[language-server.elp]
command = "elp"
args = ["server"]

[[language]]
name = "erlang"
language-servers = [ "elp" ]
```
