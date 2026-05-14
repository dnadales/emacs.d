;;; -*- lexical-binding: t; -*-
;;; repository-management-config.el
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:

(use-package magit)

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browsing links at the remote repository
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/rmuslimov/browse-at-remote
(use-package browse-at-remote
  :init
  ;; Use permanent links (permalinks)
  (setq browse-at-remote-prefer-symbolic nil)
  :bind (("C-c g y" . browse-at-remote)))

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

;;; repository-management-config.el ends here
