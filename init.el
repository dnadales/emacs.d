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
;; Set paths to executables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'exec-path "/home/damian/gems/bin")
(add-to-list 'exec-path "/home/damian/.cabal/bin")
(add-to-list 'exec-path "/home/damian/.ghcup/bin")
(add-to-list 'exec-path "/home/damian/.nix-profile/bin")
(add-to-list 'exec-path "/home/damian/.local/bin")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We want to load envrc before any other package, therefore we place it in the init file.
(use-package envrc
  :straight t
  :config (envrc-global-mode))

;; We might need this package in multiple modes, so we install it here.
(straight-use-package 'visual-fill-column)

(load "~/.emacs.d/ui-config.el")
(load "~/.emacs.d/files-config.el")
(load "~/.emacs.d/completion-config.el")
(load "~/.emacs.d/windows-management-config.el")
(load "~/.emacs.d/repository-management-config.el")
(load "~/.emacs.d/programming-config.el")
(load "~/.emacs.d/org-config.el")
(load "~/.emacs.d/roam-config.el")
(load "~/.emacs.d/haskell-config.el")
(load "~/.emacs.d/markdown-config.el")

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
 '(confirm-kill-emacs 'yes-or-no-p)
 '(custom-safe-themes
   '("833ddce3314a4e28411edf3c6efde468f6f2616fc31e17a62587d6a9255f4633" "57a29645c35ae5ce1660d5987d3da5869b048477a7801ce7ab57bfb25ce12d3e" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "603a831e0f2e466480cdc633ba37a0b1ae3c3e9a4e90183833bc4def3421a961" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "524fa911b70d6b94d71585c9f0c5966fe85fb3a9ddd635362bfabd1a7981a307" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" default))
 '(js-indent-level 2)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(safe-local-variable-values '((haskell-stylish-on-save))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(modus-themes-lang-note ((t (:underline nil))) t)
 '(modus-themes-lang-warning ((t (:underline nil))) t)
 '(org-headline-done ((((class color) (min-colors 16) (background light)) (:strike-through nil :foreground "rosy brown")))))
