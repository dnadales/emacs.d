;;; haskell-config.el --- Configuration for Haskell support
;;; server protocol.
;;;
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:

(use-package haskell-mode
  :straight t)

(use-package lsp-mode
  :straight t
  :init
  ;; Set prefix for lsp-command-keymap
  (setq lsp-keymap-prefix "C-c o")
  :hook (;; Major mode support
         (haskell-mode . lsp-deferred)
         ;; 'which-key' integration
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  ;; Setting 'gc-cons-threshold' and 'read-process-output-max' was
  ;; suggested by 'lsp-doctor'. The values were inherited from an old
  ;; config.
  ;;
  ;; This seems so solve the following problem:
  ;;
  ;;   I can't find symbols with this setup
  ;;   lsp-request: Timeout while waiting for response.  Method: textDocument/definition
  ;;
  (gc-cons-threshold 100000000)
  (read-process-output-max (* 1024 1024))
  :commands lsp)

(use-package lsp-haskell
  :straight t
  :defer t
  :requires envrc
  :custom
  (lsp-haskell-server-path "haskell-language-server"))

(use-package consult-lsp
  :straight t
  :defer t)

(use-package lsp-ui
  :straight t
  :custom
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-position 'top)
  :hook
  (lsp-mode . lsp-ui-mode))

(use-package company
  :straight t
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :straight t
  :hook (company-mode . company-box-mode))

;;; haskell-config.el ends here
