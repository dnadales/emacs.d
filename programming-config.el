;;; -*- lexical-binding: t; -*-
;;; programming-config.el
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:

(use-package vterm)

;; direnv integration: automatically set buffer-local environment from
;; .envrc files so that LSP, compilation, and vterm pick up per-project
;; nix-shell / nix-develop environments.
(use-package envrc
  :config
  (envrc-global-mode))

;;; programming-config.el ends here
