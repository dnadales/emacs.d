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

;; org-roam-ui configuration
(add-to-list 'load-path "~/.emacs.d/elisp/org-roam-ui")
(load-library "org-roam-ui")
(require 'org-roam-protocol)

;; Deft provides an interface for browsing and filtering org-roam notes.
;;
;; WARNING: this package can be slow when the number of files becomes large. In
;; such case you might want to try: https://github.com/hasu/notdeft
(use-package deft
  :after org
  :bind
  ("C-c r d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))
