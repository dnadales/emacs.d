;;; -*- lexical-binding: t; -*-
;;; programming-config.el
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(use-package vterm)

;; direnv integration: automatically set buffer-local environment from
;; .envrc files so that LSP, compilation, and vterm pick up per-project
;; nix-shell / nix-develop environments.
(use-package envrc
  :config
  (envrc-global-mode))

;; Semantic selection expansion: repeated C-= widens the selection
;; from word -> sentence -> paragraph -> function body, etc.
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;; programming-config.el ends here
