---
sidebar_position: 4
---

# Vim

## Via Pathogen Package Manager

### Install the Pathogen package manager

You can install the [Pathogen](https://github.com/tpope/vim-pathogen) package manager by running:

```
mkdir -p ~/.vim/autoload ~/.vim/bundle && \
curl -LSso ~/.vim/autoload/pathogen.vim https://tpo.pe/pathogen.vim
```

Then add the following to your `~/.vimrc` file:

```
execute pathogen#infect()
```

### Install the `vim-lsp` plugin

In a terminal, run:

```
cd ~/.vim/bundle
git clone https://github.com/prabirshrestha/vim-lsp.git
```

Then add the following to your `~/.vimrc` file:

```
if executable('elp')
    au User lsp_setup call lsp#register_server({
        \ 'name': 'elp',
        \ 'cmd': {server_info->['elp', 'server']},
        \ 'allowlist': ['erlang'],
        \ })
endif
```

### Install auto-completion

You can install the following plugins to get autocompletion via the ELP language server:

```
git clone https://github.com/prabirshrestha/asyncomplete.vim.git
git clone https://github.com/prabirshrestha/asyncomplete-lsp.vim.git
```

For more information and customizations, please refer to the official
 [Pathogen](https://github.com/tpope/vim-pathogen#installation) and
 [lsp-vim](https://github.com/prabirshrestha/vim-lsp) documentation pages.

## Troubleshooting

`:LspStatus` shows the current status of the LSP server.
