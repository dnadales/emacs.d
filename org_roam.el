(setq org-roam-v2-ack t)
(require 'org-roam)
(setq org-roam-directory (file-truename "~/Dropbox/docs/org/roam/"))

(org-roam-setup)

;;; Define key bindings for Org-roam
(global-set-key (kbd "C-c r r") #'org-roam-buffer-toggle)
(global-set-key (kbd "C-c r b") #'org-roam-buffer)
(global-set-key (kbd "C-c r i") #'org-roam-node-insert)
(global-set-key (kbd "C-c r c") #'org-roam-capture)
(global-set-key (kbd "C-c r /") #'org-roam-node-find)
