;;; repository-management-config.el
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:

(straight-use-package 'magit)

(straight-use-package 'projectile)
;; Enable projectile globally.
(projectile-global-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browsing links at the remote repository
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/rmuslimov/browse-at-remote
(use-package browse-at-remote
  :straight t
  :init
    ;; Use permanent links (permalinks)
    (setq browse-at-remote-prefer-symbolic nil)
  :bind (("C-x p y" . browse-at-remote))
 )

;; repository-management-config.el ends here
