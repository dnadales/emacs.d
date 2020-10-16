;;; haskell_lsp.el --- Configuration for haskell language server via language
;;; server protocol.
;;;
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:

(require 'lsp)
(require 'lsp-haskell)
;; Hooks so haskell and literate haskell major modes trigger LSP setup
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

;;; haskell_lsp.el ends here
