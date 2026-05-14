;;; -*- lexical-binding: t; -*-
;;; roam-config.el
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:

(use-package org-roam
  :config
  (setq org-roam-directory (file-truename "~/psys/roam/"))
  (org-roam-db-autosync-mode))

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
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;;; roam-config.el ends here
