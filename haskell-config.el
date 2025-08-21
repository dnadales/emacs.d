;;; haskell-config.el --- Configuration for Haskell support
;;;
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:



(straight-use-package 'use-package)
(setq straight-use-package-by-default t)




(let ((ghcup-path (concat (getenv "HOME") "/.ghcup/bin")))
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
  (lsp-enable-snippet t)
  (lsp-haskell-server-path "haskell-language-server-wrapper"))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

(setq lsp-haskell-plugin-stan-global-on nil)



(use-package lsp-haskell
  :after lsp-mode
  :custom
  (lsp-haskell-server-path "haskell-language-server-wrapper"))

(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)






;; (use-package lsp-mode
;;   :straight t
;;   :init
;;   :hook (;; ;; Major mode support
;;          ;; (haskell-mode . lsp-deferred)
;; 	 ;; (lsp-mode . yas-minor-mode)
;;          (lsp-mode . lsp-enable-which-key-integration)
;; 	 )
;;   :custom
;;   ;; Setting 'gc-cons-threshold' and 'read-process-output-max' was
;;   ;; suggested by 'lsp-doctor'. The values were inherited from an old
;;   ;; config.
;;   ;;
;;   ;; This seems so solve the following problem:
;;   ;;
;;   ;;   I can't find symbols with this setup
;;   ;;   lsp-request: Timeout while waiting for response.  Method: textDocument/definition
;;   ;;
;;   (gc-cons-threshold 100000000)
;;   (read-process-output-max (* 1024 1024))
;;   (lsp-keymap-prefix "C-c l")
;;   (lsp-enable-suggest-server-download nil)
;;   ;; :bind (:map lsp-mode-map ("M-RET" . lsp-execute-code-action))
;;   :commands lsp)

;; (use-package lsp-haskell
;;   :straight t
;;   :defer t
;;   ;; :requires envrc
;;   :custom
;;   (lsp-haskell-session-loading "multipleComponents")
;;   ;; Disable Stan static analysis plugin in Haskell Language Server
;;   (lsp-haskell-plugin-stan-global-on nil))

;; (use-package haskell-mode
;;   :straight t
;;   :hook
;;   (haskell-mode . lsp))

;; ;; (use-package consult-lsp
;; ;;   :straight t
;; ;;   :defer t)

;; ;; (use-package lsp-ui
;; ;;   :straight t
;; ;;   :custom
;; ;;   (lsp-ui-doc-header t)
;; ;;   (lsp-ui-doc-include-signature t)
;; ;;   (lsp-ui-doc-position 'top)
;; ;;   :hook
;; ;;   (lsp-mode . lsp-ui-mode))

;; (use-package company
;;   :straight t
;;   :after lsp-mode
;;   :hook (prog-mode . company-mode)
;;   :bind (:map company-active-map
;;          ("<tab>" . company-complete-selection))
;;         (:map lsp-mode-map
;;          ("<tab>" . company-indent-or-complete-common))
;;   :custom
;;   (company-minimum-prefix-length 1)
;;   (company-idle-delay 0.0))

;; (use-package company-box
;;   :straight t
;;   :hook (company-mode . company-box-mode))

;; ;;; haskell-config.el ends here
