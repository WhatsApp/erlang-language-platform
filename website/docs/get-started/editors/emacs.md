---
sidebar_position: 2
---

# Emacs

The ELP project can be used as a
[language server](https://microsoft.github.io/language-server-protocol/overviews/lsp/overview/)
in the Emacs text editor via the [eglot](https://github.com/joaotavora/eglot) or
[lsp-mode](https://emacs-lsp.github.io/lsp-mode/) LSP clients.

## Eglot

Eglot is part of Emacs core since Emacs 29. For earlier versions it can be
installed with the `eglot` package.

### Configuration

```elisp
(use-package eglot
  :ensure t
  :hook ((erlang-mode . eglot-ensure))

  :config
  ;; Remove default LSP server
  (setopt eglot-server-programs
          (assq-delete-all 'erlang-mode eglot-server-programs))

  ;; Enable ELP
  (add-to-list 'eglot-server-programs
               '(erlang-mode . ("elp" "server"))))
```

Refer to the
[manual](https://elpa.gnu.org/devel/doc/eglot.html#Customization-Variables) for
additional configuration options.

### Semantic tokens

Semantic token support has been added to eglot, but is not yet in the
released version. But it is possible to install the updated version
of eglot.

To do so, add

```elisp
(add-to-list 'package-archives '("gnu-devel" . "https://elpa.gnu.org/devel/"))
```

to your `init.el`, then run `M-x eglot-upgrade-eglot`

Once upgraded, add the following to the `(use-package` entry for `eglot`

```elisp
  (setq eglot-semantic-token-modifiers '
    ("bound" "exported_function" "exported_type" "deprecated_function" "type_dynamic"))

  ;; Each face name arises as a template from the modifiers as
  ;; "eglot-semantic-%s-face"
  (defface eglot-semantic-bound-face
    '((t :underline t))
    "The face modification to use for bound variables in patterns."
    :group 'eglot-semantic-semantic-tokens)

  (defface eglot-semantic-exported_function-face
    '((t :underline t))
    "The face modification to use for exported functions."
    :group 'eglot-semantic-semantic-tokens)

  (defface eglot-semantic-exported_type-face
    '((t :underline t))
    "The face modification to use for exported types."
    :group 'eglot-semantic-semantic-tokens)

  (defface eglot-semantic-deprecated_function-face
    '((t :strike-through t))
    "The face modification to use for deprecated functions."
    :group 'eglot-semantic-semantic-tokens)

  (defface eglot-semantic-type_dynamic-face
    '((t (:weight bold)))
    "The face modification to use for dynamic types."
    :group 'eglot-semantic-semantic-tokens)

  ;; Bare eglot makes the refresh a no-op. Provide our own version for
  ;; when Eqwalizer gets its results.
  (cl-defmethod eglot-handle-request
    (server (_method (eql workspace/semanticTokens/refresh)) &rest args)
    "Handle workspace/semanticTokens/refresh by refreshing font-lock."
    (dolist (buffer (eglot--managed-buffers server))
      (eglot--when-live-buffer buffer
        (eglot--widening (font-lock-flush)))))
```


## lsp-mode

Install the `lsp-mode` package, which is a generic Emacs client for LSP servers.
You can follow
[these instructions](https://emacs-lsp.github.io/lsp-mode/page/installation/) to
install it.

### Configuration

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

For a list of available configuration option, please refer to
[this page](https://emacs-lsp.github.io/lsp-mode/page/lsp-erlang-elp/) and to
the
[`lsp-mode` settings documentation](https://emacs-lsp.github.io/lsp-mode/page/settings/mode/).

There is also a
[`.dotemacs`](https://github.com/WhatsApp/erlang-language-platform/blob/main/editors/emacs/dotemacs.el)
file in the ELP repository that you can use as a reference.

## Troubleshooting

#### The following servers support current file but do not have automatic installation

Ensure that the `elp` executable is available in your `PATH` via Emacs. A
workaround is:

```elisp
;; Ensure your Emacs environment looks like your user's shell one
(package-require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
```
