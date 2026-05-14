;;; -*- lexical-binding: t; -*-
;;; completion-config.el
;;; Author: Damian Nadales
;;;
;;; Code:

(use-package vertico
  :config
  (vertico-mode))

(savehist-mode)

(recentf-mode 1)
(setq recentf-max-menu-items 100000)
(setq recentf-max-saved-items 100000)

;; See https://github.com/minad/consult#use-package-example
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-x bindings in `ctl-x-map'
         ("C-x b"   . consult-buffer)              ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer

         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop

         ;; M-g bindings in `goto-map'
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o"   . consult-outline)             ;; Alternative: consult-org-heading
         ("M-g m"   . consult-mark)
         ("M-g k"   . consult-global-mark)

         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Contextual actions on minibuffer candidates and at-point targets.
;; Completes the vertico/consult/marginalia/orderless stack.
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Editable grep buffers: consult-ripgrep -> embark-export -> wgrep -> save
(use-package wgrep)

;; which-key is built-in since Emacs 30.
(which-key-mode)

;;; completion-config.el ends here
