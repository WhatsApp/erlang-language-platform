---
sidebar_position: 5
---

# Emacs

The ELP project can be used as a [language server](https://microsoft.github.io/language-server-protocol/overviews/lsp/overview/) in the Emacs text editor via the [lsp-mode](https://emacs-lsp.github.io/lsp-mode/) LSP client.

## Requirements

### `lsp-mode`

First, you need the `lsp-mode` package, which is an Emacs client for LSP servers. You can follow [these instructions](https://emacs-lsp.github.io/lsp-mode/page/installation/) to install it.

### `rebar3` `build-info` plugin

This plugin is required by `ELP` to auto-discover the structure of `rebar3` based projects. To enable it globally, add the following lines to your `~/.config/rebar3/rebar.config` file:

```erlang
{project_plugins, [
  {eqwalizer_rebar3,
    {git_subdir,
        "https://github.com/whatsapp/eqwalizer.git",
        {branch, "main"},
        "eqwalizer_rebar3"}}
]}.
```

## Install ELP

Please refer to the [installation instructions](./install.md) page.

## Configure Emacs

Add the following to your emacs `.emacs` file or equivalent.

```elisp
(use-package lsp-mode

  :config
  ;; Enable LSP automatically for Erlang files
  (add-hook 'erlang-mode-hook #'lsp)

  ;; ELP, added as priority 0 (> -1) so takes priority over the built-in one
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("elp" "server"))
                    :major-modes '(erlang-mode)
                    :priority 0
                    :server-id 'erlang-language-platform))
  )
```

For a list of available configuration option, please refer to [this page](https://emacs-lsp.github.io/lsp-mode/page/lsp-erlang-elp/) and to the [`lsp-mode` settings documentation](https://emacs-lsp.github.io/lsp-mode/page/settings/mode/).

## Troubleshooting

#### The following servers support current file but do not have automatic installation

Ensure that the `elp` executable is available in your `PATH` via Emacs. A workaround is:

```elisp
;; Ensure your Emacs environment looks like your user's shell one
(package-require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
```

#### build-info plugin was not installed

You need the `build-info` plugin for `rebar3` installed. See [how to install it](#rebar3-build-info-plugin).

## Support

Things did not work as expected? We are ready to help in the [#elp](https://erlanger.slack.com/archives/C05KJHDJJUF) Slack channel.
