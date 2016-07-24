;;; helm_config.el -- Configuration for helm.
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:

(require 'helm)
(require 'helm-config)
(require 'helm-grep)
(require 'helm-files)

;; Make helm autoresize the completion buffers based on the number of
;; candidates.
(helm-autoresize-mode 1)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

;; Open helm buffer inside current window, not occupy whole other
;; window:
(setq
 helm-split-window-in-side-p t
 ;; Move to end or beginning of source when reaching top or bottom of
 ;; source.
 helm-move-to-line-cycle-in-source t
 ;; search for library in `require' and `declare-function' sexp.
 helm-ff-search-library-in-sexp t
 helm-ff-file-name-history-use-recentf t)

(setq helm-M-x-fuzzy-match t)

(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(global-set-key (kbd "C-x b") 'helm-mini)

(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Enable helm globally
(helm-mode 1)

;;; helm_config.el ends here
