;; -*- lisp -*-

;; This file is an adaptation of the erlang_ls one at
;; https://github.com/erlang-ls/erlang_ls/blob/main/misc/dotemacs

;; You can test it by running
;;
;;  emacs -q --load dotemacs.el
;;
;; Note:
;; 1. The first time you open a new WASERVER repo, use the "choose
;;    server root ineractively" option to choose
;;    "~/local/whatsapp/server/erl" (or devserver equivalent) as the root.
;; 2. When you first start up the server, emacs becomes unresponsive
;;    for about a minute.  This is not an officially supported platform, sorry.


;; ---------------------------------------------------------------------

;; This is a sample .emacs file which you can use to troubleshoot your
;; ELP/Erlang LS Emacs setup.

;; Use packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

;; Define a utility function which either installs a package (if it is
;; missing) or requires it (if it already installed).
(defun package-require (pkg &optional require-name)
  "Install a package only if it's not already installed."
  (when (not (package-installed-p pkg))
    (package-install pkg))
  (if require-name
      (require require-name)
    (require pkg)))


;; Install the official Erlang mode
(package-require 'erlang)

;; Customize prefix for key-bindings
;; This has to be done before lsp-mode itself is loaded
(setq lsp-keymap-prefix "C-l")

;; Include the Language Server Protocol Clients
(package-require 'lsp-mode)

;; ---------------------------------------------------------------------
;; Define the ELP/erlang_ls language server

(defgroup lsp-erlang-elp nil
  "LSP support for the Erlang programming language, using ELP/erlang-ls"
  :group 'lsp-mode
  :link '(url-link "https://github.com/WhatsApp/erlang-language-platform"))

(defcustom lsp-erlang-elp-server-path
  "~/local/whatsapp/server/erl/tools/elp"
  "Path to the ELP binary."
  :group 'lsp-erlang-elp
  :risky t
  :type 'file)

(defcustom lsp-erlang-elp-erlang-ls-path
  "~/fbsource/xplat/vscode/vscode-extensions/erlang/bin/erlang_ls"
  "Path to the Erlang Language Server (erlang_ls) binary."
  :group 'lsp-erlang-elp
  :risky t
  :type 'file)


(defun lsp-erlang-elp-server-connection ()
  (lsp-stdio-connection
   `(,lsp-erlang-elp-server-path
     "--sub-server-cmd" ,lsp-erlang-elp-erlang-ls-path
     "--log-file", "~/tmp/elp.log",
     "--no-log-buffering")))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-erlang-elp-server-connection)
                  :major-modes '(erlang-mode)
                  :priority 0 ;; built-in erlang_ls is -1, choose this instead
                  :server-id 'erlang-elp))

(lsp-consistency-check lsp-erlang-elp)

(provide 'lsp-erlang-elp)

;; ---------------------------------------------------------------------


;; Enable LSP for Erlang files
(add-hook 'erlang-mode-hook #'lsp)

;; Require and enable the Yasnippet templating system
(package-require 'yasnippet)
(yas-global-mode t)

;; Disable logging for lsp-mode
(setq lsp-log-io nil)

;; Show line and column numbers
(add-hook 'erlang-mode-hook 'linum-mode)
(add-hook 'erlang-mode-hook 'column-number-mode)

;; Enable and configure the LSP UI Package
(package-require 'lsp-ui)
(setq lsp-ui-sideline-enable t)
(setq lsp-ui-doc-enable t)
(setq lsp-ui-doc-position 'bottom)

;; Enable LSP Origami Mode (for folding ranges)
(package-require 'lsp-origami)
(add-hook 'origami-mode-hook #'lsp-origami-mode)
(add-hook 'erlang-mode-hook #'origami-mode)

;; Provide commands to list workspace symbols:
;; - helm-lsp-workspace-symbol
;; - helm-lsp-global-workspace-symbol
(package-install 'helm-lsp)

;; Which-key integration
(package-require 'which-key)
(add-hook 'erlang-mode-hook 'which-key-mode)
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; Always show diagnostics at the bottom, using 1/3 of the available space
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
              (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side            . bottom)
              (reusable-frames . visible)
              (window-height   . 0.33)))
