;;; files-config.el
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;; Configuration related to file handling and file system configurations.
;;;
;;; Code:


;;
;; Backup settings
;;
(setq
   backup-by-copying t               ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/backups/")) ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)                ; use versioned backups

;; Delete trailing whitespace on save, globally.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; files-config.el ends here
