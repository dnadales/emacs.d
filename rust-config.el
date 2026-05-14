;;; -*- lexical-binding: t; -*-
;;; rust-config.el --- Configuration for Rust support
;;;
;;; Author: Damian Nadales
;;;
;;; Code:

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t)
  :config
  (setq rust-format-on-save t)
  :hook (rust-mode . lsp))

;;; rust-config.el ends here
