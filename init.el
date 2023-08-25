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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We want to load envrc before any other package, therefore we place it in the init file.
(use-package envrc
  :straight t
  :config (envrc-global-mode))

(load "~/.emacs.d/ui-config.el")
(load "~/.emacs.d/files-config.el")
(load "~/.emacs.d/completion-config.el")
(load "~/.emacs.d/windows-management-config.el")
(load "~/.emacs.d/repository-management-config.el")
(load "~/.emacs.d/programming-config.el")
(load "~/.emacs.d/org-config.el")
(load "~/.emacs.d/roam-config.el")
(load "~/.emacs.d/haskell-config.el")

;; Server:
;; Use emacs as a server. See manual section 31.3 (Using emacs as a
;; server). This is quite useful for having only a single instance of
;; the editor running.
(server-start)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(modus-themes-lang-note ((t (:underline nil))) t)
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:strike-through nil :foreground "rosy brown"))))
 '(org-level-1 ((t (:foreground "dark turquoise" :height 1.0))))
 '(org-level-2 ((t (:foreground "pale turquoise"))))
 '(org-level-3 ((t (:foreground "lemon chiffon"))))
 '(org-level-4 ((t (:foreground "white smoke"))))
 '(org-level-5 ((t (:foreground "white smoke"))))
 '(org-level-6 ((t (:foreground "white smoke")))))
