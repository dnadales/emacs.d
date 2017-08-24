;;; haskell_config.el --- Configuration for haskell-mode.
;;;
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:
(require 'haskell-mode)

;; TODO: not all the features I need are supported in intero-mode
;; Left for the future.
;; (add-hook 'haskell-mode-hook 'intero-mode)

;; Using hasktags
(let ((my-cabal-path (expand-file-name "~/.local/bin")))
  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))
(custom-set-variables '(haskell-tags-on-save t))

;; To combine output from different backends in the autocompletion.
(add-hook 'haskell-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 (append '((company-capf company-dabbrev-code))
                         company-backends))))

(add-hook 'haskell-mode-hook
          (lambda ()
            (flyspell-prog-mode)))

;; stack install hindent
(require 'hindent)
(add-hook 'haskell-mode-hook #'hindent-mode)

;; Enable auto-insertion of module templates.
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

;; Enable declaration scanning. This is useful to move around declarations
;; (C-M-a, C-M-e, C-M-h).
(add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)

;; Enable "which function mode".
(eval-after-load "which-func"
  '(add-to-list 'which-func-modes 'haskell-mode))

;; Enable speed-bar support.
(require 'speedbar)
(speedbar-add-supported-extension ".hs")

;; Use stack build
(eval-after-load "haskell-mode"
    '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

(eval-after-load "haskell-cabal"
  '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

(setq haskell-compile-cabal-build-command "stack build")

(custom-set-variables
 '(haskell-process-type 'stack-ghci))

;; Haskell Interactive Mode Setup
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t))

;; Auto-remove imports
(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t))

;; Auto-adding module imports
(custom-set-variables
 '(haskell-process-suggest-hoogle-imports t))

;; Improved flycheck
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;;; haskell_config.el ends here
