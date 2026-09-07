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
  ;; :capf means "turn on company-mode" in lsp-mode. We use corfu, so tell
  ;; lsp to configure no popup. It still adds lsp-completion-at-point to
  ;; completion-at-point-functions, which corfu reads.
  (lsp-completion-provider :none)
  (lsp-log-io nil)
  (lsp-enable-snippet t)
  ;; lsp-mode registers these clients for every buffer it can match, then
  ;; reports the missing binary. rls is dead, and semgrep is not installed.
  (lsp-disabled-clients '(rls semgrep-ls)))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-haskell
  :after lsp-mode
  :custom
  (lsp-haskell-server-path "haskell-language-server-wrapper")
  (lsp-haskell-plugin-stan-global-on nil))

(add-hook 'haskell-literate-mode-hook #'lsp)

(put 'haskell-stylish-on-save 'safe-local-variable #'booleanp)

;;; haskell-config.el ends here
