---
sidebar_position: 5
---

# Neovim

![screenshot](/img/neovim.png)

The ELP project can be used as a [language server](https://microsoft.github.io/language-server-protocol/overviews/lsp/overview/) in Neovim.

## Installation

### `mason.nvim`

The easiest way to install the ELP server is to use [mason.nvim](https://github.com/williamboman/mason.nvim), a package manager
focused on language servers, linters and similar tools. If you use Neovim for programming, you likely already have `mason.nvim` installed. It is also included in all the most popular "batteries included" configurations/distributions of Neovim such [kickstart.nvim](https://github.com/nvim-lua/kickstart.nvim), [LazyVim](https://github.com/LazyVim/LazyVim), [NVChad](https://nvchad.com/) and others.

With `mason.nvim` installed, you can install the ELP server by running the following command:
```
:MasonInstall elp
```
from within Neovim.

Alternatively you can run `:Mason` and browse the list of available packages, then press `i` when `elp` is highlighted.

#### Updating ELP

You can update all packages managed by `mason.nvim` by running `:Mason` and pressing `U`.

### Manual installation

Alternatively, if you don't have `mason.nvim` installed, you can [install the ELP binary](../install.md#from-binary) and configure key mappings manually.

#### Example config

Here is an example user config to be placed in your `init.vim` file (`~/.config/nvim/init.vim` on Unix systems):
```Vimscript
nnoremap <SPACE> <Nop>
let mapleader=" "

call plug#begin()
Plug 'neovim/nvim-lspconfig'
call plug#end()

lua <<EOF
local opts = { noremap=true, silent=true }
vim.api.nvim_set_keymap('n', '<leader>e', '<cmd>lua vim.diagnostic.open_float()<CR>', opts)

local on_attach = function(client, bufnr)
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  if client.server_capabilities.inlayHintProvider then
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>ih',
      '<cmd>lua vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())<CR>',
      { noremap=true, silent=true, desc = '[T]oggle Inlay [H]ints' })
  end
end

require('lspconfig').elp.setup {
  on_attach = on_attach,
  settings = {
    elp = {
      diagnostics = {
        disabled = {
          "W0030",
          "W0031",
          "W0032"
        }
      }
    }
  }
}
EOF
```

In this example config, the Leader key is mapped to `<space>`.

[vim-plug](https://github.com/junegunn/vim-plug), a minimalist Vim plugin manager is used to install [nvim-lspconfig](https://github.com/neovim/nvim-lspconfig).

The Lua code then defines the following key mappings in "normal" Vim mode:
* `<space>e` displays diagnostics, for example the red text in above screenshot
* `gd` goes to the definition
* `K` shows function specs
* `<space>rn` renames a variable
* `gr` shows references
* `<space>ca` suggests code actions for warnings displayed ELP
* `<space>ih` toggles displaying inlay hints (for example function argument hints)

Finally, you can disable some [warnings](../../erlang-error-index/w/about.md) globally for all your Erlang projects.

## Troubleshooting

`:LspInfo` shows the current status of the LSP client.

`:LspLog` opens the log file for the LSP client, which can be useful for debugging.
