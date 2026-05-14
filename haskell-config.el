;;; -*- lexical-binding: t; -*-
;;; haskell-config.el --- Configuration for Haskell support
;;;
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:

(let ((ghcup-path (expand-file-name ".ghcup/bin" "~")))
  (setenv "PATH" (concat ghcup-path ":" (getenv "PATH")))
  (add-to-list 'exec-path ghcup-path))

(use-package haskell-mode
  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . lsp))
  :config
  (setq haskell-process-type 'cabal-repl))

(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-completion-provider :capf)
  (lsp-log-io nil)
  (lsp-enable-snippet t))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-haskell
  :after lsp-mode
  :custom
  (lsp-haskell-server-path "haskell-language-server-wrapper")
  (lsp-haskell-plugin-stan-global-on nil))

(add-hook 'haskell-literate-mode-hook #'lsp)

;;; haskell-config.el ends here
