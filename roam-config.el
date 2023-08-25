;;; roam-config.el
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:

(use-package org-roam
  :straight t
  :ensure t
  )

(setq org-roam-directory (file-truename "~/psys/roam/"))
(org-roam-setup)
(org-roam-db-autosync-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-c r r") #'org-roam-buffer-toggle)
(global-set-key (kbd "C-c r b") #'org-roam-buffer)
(global-set-key (kbd "C-c r i") #'org-roam-node-insert)
(global-set-key (kbd "C-c r c") #'org-roam-capture)
(global-set-key (kbd "C-c r /") #'org-roam-node-find)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-roam-ui
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;;; roam-config.el
