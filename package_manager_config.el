;;; package_manager_config.el  --- Package management configuration.
;;;
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;; Configuration related to package management.
;;;
;;; Code:

(require 'package)

(setq
 use-package-always-ensure t
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;		    ("melpa-stable" . "https://stable.melpa.org/packages/")
                    ("melpa" . "http://melpa.org/packages/"))
 )

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

;; Path to my own packages.
(add-to-list 'load-path "~/.emacs.d/elisp/")

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Required packages
(use-package haskell-mode)
(use-package projectile)
(use-package haskell-snippets)
(use-package helm-projectile)
(use-package buffer-move)
(use-package indent-tools)
(use-package hindent)
(use-package flycheck-haskell)
(use-package helm-xref)
(use-package markdown-mode)
(use-package mmm-mode)
(use-package color-theme-sanityinc-tomorrow)
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))
(use-package pdf-tools)
;; direnv mode setup
(use-package direnv
  :hook
  (eshell-directory-change . direnv-update-directory-environment)
  :config
  (direnv-mode))
(use-package flycheck
  :custom
  (flycheck-executable-find (lambda (cmd)
			      (direnv-update-environment default-directory)
			      (executable-find cmd)))
  ;; :bind
  ;; (("<f9>" . flycheck-next-error)
  ;;  ("<f10>" . flycheck-previous-error))
  :hook
  (prog-mode . flycheck-mode))

;; We need direnv to execute before lsp starts. So we start lsp in deferred
;; mode when opening haskell files.
;; (use-package lsp-mode
;;   :hook (haskell-mode . lsp-deferred)
;;   :diminish lsp-mode
;;   :custom
;;   (lsp-keymap-prefix "M-s-l")
;;   :commands (lsp lsp-deferred))
;; (use-package lsp-ui)
;; (use-package lsp-haskell)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; I got this configuration from https://dev.to/deciduously/how-i-emacs-and-so-can-you-packages-m9p
(use-package which-key
  :init
  (which-key-mode)
  :config
  ;; (which-key-setup-side-window-right-bottom)
  ;; (setq which-key-sort-order 'which-key-key-order-alpha
  ;;   which-key-side-window-max-width 0.33
  ;;   which-key-idle-delay 0.05)
  ;; :diminish which-key-mode
  )

;;; package_manager_config.el ends here
