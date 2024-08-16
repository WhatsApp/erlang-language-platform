---
sidebar_position: 6
---

# Helix

![screenshot](/img/helix.png)

The ELP project can be used as a [language server](https://microsoft.github.io/language-server-protocol/overviews/lsp/overview/)
in Helix via its native LSP support.

To configure it, simply add or modify the Erlang [language-specific settings](https://docs.helix-editor.com/languages.html)
via your `~/.config/helix/languages.toml` file or equivalent, as follows:

```
[language-server.elp]
command = "elp"
args = ["server"]

[[language]]
name = "erlang"
language-servers = [ "elp" ]
```
