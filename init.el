;;; -*- lexical-binding: t; -*-
;;; init.el
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;; Entry point for my Emacs configuration.
;;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package manager bootstrapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://github.com/radian-software/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set paths to executables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (path '("gems/bin" ".cabal/bin" ".ghcup/bin" ".nix-profile/bin" ".local/bin"))
  (add-to-list 'exec-path (expand-file-name path "~")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load (expand-file-name "ui-config.el" user-emacs-directory))
(load (expand-file-name "files-config.el" user-emacs-directory))
(load (expand-file-name "completion-config.el" user-emacs-directory))
(load (expand-file-name "windows-management-config.el" user-emacs-directory))
(load (expand-file-name "repository-management-config.el" user-emacs-directory))
(load (expand-file-name "programming-config.el" user-emacs-directory))
(load (expand-file-name "org-config.el" user-emacs-directory))
(load (expand-file-name "roam-config.el" user-emacs-directory))
(load (expand-file-name "haskell-config.el" user-emacs-directory))
(load (expand-file-name "rust-config.el" user-emacs-directory))
(load (expand-file-name "markdown-config.el" user-emacs-directory))
(load (expand-file-name "quint-config.el" user-emacs-directory))

;; Additional editor configurations
(setq sentence-end-double-space nil) ;; Sentences end with a period. Period.

(global-set-key (kbd "C-<backspace>") 'backward-kill-word)

;; Tabs are evil
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Built-in quality-of-life modes
(global-auto-revert-mode 1)      ; refresh buffers when files change on disk
(save-place-mode 1)              ; remember cursor position per file
(electric-pair-mode 1)           ; auto-close parens, brackets, quotes
(pixel-scroll-precision-mode 1)  ; smooth scrolling (pgtk)

;; Server:
;; Use emacs as a server. See manual section 31.3 (Using emacs as a
;; server). This is quite useful for having only a single instance of
;; the editor running.
(server-start)

;;; init.el ends here
