---
sidebar_position: 4
---

# Neovim

![screenshot](/img/neovim.png)

The ELP project can be used as a [language server](https://microsoft.github.io/language-server-protocol/overviews/lsp/overview/) in Neovim.

The easiest way to install the ELP server is to use [mason.nvim](https://github.com/williamboman/mason.nvim), a package manager
focused on language servers, linters and similar tools. If you use Neovim for programming, you likely already have `mason.nvim` installed. It is also included in all the most popular "batteries included" configurations/distributions of Neovim such [kickstart.nvim](https://github.com/nvim-lua/kickstart.nvim), [LazyVim](https://github.com/LazyVim/LazyVim), [NVChad](https://nvchad.com/) and others.

With `mason.nvim` installed, you can install the ELP server by running the following command:
```
:MasonInstall elp
```
from within Neovim.

Alternatively you can run `:Mason` and browse the list of available packages, then press `i` when `elp` is highlighted.

## Updating ELP

You can update all packages managed by `mason.nvim` by running `:Mason` and pressing `U`.

## Troubleshooting

`:LspInfo` shows the current status of the LSP client.

`:LspLog` opens the log file for the LSP client, which can be useful for debugging.


