;;; haskell_lsp.el --- Configuration for haskell language server via language
;;; server protocol.
;;;
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:

(use-package lsp-mode
  :hook
  (haskell-mode . lsp)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (gc-cons-threshold 100000000)
  (read-process-output-max (* 1024 1024))
  :bind
  (:map lsp-mode-map
        ("M-." . lsp-ui-peek-find-definitions)
        ("C-M-." . lsp-ui-peek-find-references)
        ("<f5>" . lsp-ui-imenu)
        ("<f6>" . consult-lsp-symbols)
;;        ("<f7>" . lsp-treemacs-errors-list)
        ("<f8>" . lsp-execute-code-action))
  :config
  (push "[/\\\\]\\.stack-work\\'" lsp-file-watch-ignored-directories)
  (advice-add 'lsp :before #'direnv-update-environment))

(use-package lsp-haskell
  :defer t
  :custom
  (lsp-haskell-server-path "haskell-language-server"))

(use-package consult-lsp
  :defer t)

(use-package lsp-ui
  :custom
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  :hook
  (lsp-mode . lsp-ui-mode))

;; Hooks so haskell and literate haskell major modes trigger LSP setup
;; (add-hook 'haskell-mode-hook #'lsp)
;; (add-hook 'haskell-literate-mode-hook #'lsp)

;;; haskell_lsp.el ends here
