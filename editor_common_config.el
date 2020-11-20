;;; editor_common_config.el --- Common configuration for all editing modes.
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;; The settings here are global to all Emacs edditing modes.
;;;
;;; Code:

;; Set a theme, if the default one is not desired.
;; (load-theme 'sanityinc-tomorrow-day)
;;(load-theme 'sanityinc-tomorrow-bright)
;;(load-theme 'sanityinc-tomorrow-night)
(load-theme 'tango-dark)
;; (load-theme 'wombat)
;; (load-theme 'espresso t)
;; (load-theme 'adwaita)
;; (load-theme 'dichromacy)
;;(load-theme 'whiteboard)
;; (load-theme 'tango)
;; (load-theme 'leuven)
;;(invert-face 'default)
;; (load-theme 'solarized-light t)
;;(load-theme 'solarized-dark t)
;; (load-theme 'solarized-gruvbox-light)
;; (load-theme 'flatui t)
;; (load-theme 'alect-dark t)

;; auto close bracket insertion. New in emacs 24
(electric-pair-mode 1)

;; Turn on highlight of matching brackets when cursor is on one of them.
(show-paren-mode 1)

;; Sentence end:
;; A sentence should be ended by a period. Period.
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Sentences.html
(setq sentence-end-double-space nil)

;; Indentation configuration: no tabs, and 4 use characters for indentation.
(setq-default
 indent-tabs-mode nil
 tab-width 4
 c-basic-offset 4)

;; Text modes hooks.
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-off-auto-fill)

;; Columns:
;; Set the number to the number of columns to use.
(setq-default fill-column 79)

;; Splash screen:
;; Bye bye startup screen.
(setq inhibit-splash-screen t)

;; Menubar:
(menu-bar-mode t)
;; Scrollbar:
(set-scroll-bar-mode nil)
;; Toolbar:
(tool-bar-mode -1)

;; Fonts:
;;
(cond
 ((string-equal system-type "gnu/linux")
  ;; (set-frame-font "Hack-18") ;; sudo apt-get install fonts-hack-ttf
  ;; Other nice fonts I've used in the past:
  ;; (set-frame-font "DejaVu Sans Mono-16")
  ;;(set-frame-font "Ubuntu Mono-18")
  ;;(set-frame-font "Fira Code-16") ;; sudo apt-get install fonts-firacode
  (set-frame-font "JetBrains Mono-14") ;; https://www.jetbrains.com/lp/mono/#how-to-install
  ;; (set-frame-font "Inconsolata-16") ;; sudo apt-get install fonts-inconsolata
  )

 ((string-equal system-type "windows-nt")
  (set-frame-font "Consolas-12")))

(defun change-font-size (new-size)
  "Change the font size to the given value"
  (interactive "nNew font size: ")
  (set-face-attribute 'default nil :height (* 10 new-size))
  )

(bind-key "C-x f" #'change-font-size)

(defun my/adjust-font-size-based-on-display ()
  (let ((display-width (nth 3 (assq 'geometry (frame-monitor-attributes))))
        )
    (change-font-size
     (cond ((<= display-width 1920) 12) ;; HD
           ((<= display-width 2560) 11) ;; UWHD
           ((<= display-width 4096) 14) ;; 4K
           )
     )
    )
  )

(add-hook 'window-size-change-functions (lambda (frame) (my/adjust-font-size-based-on-display)))
(add-hook 'focus-in-hook 'my/adjust-font-size-based-on-display)

;; Backup settings
(setq
   backup-by-copying t               ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/backups/")) ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)                ; use versioned backups

;; Markdown mode
;; http://jblevins.org/projects/markdown-mode/
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG" . markdown-mode))

;; Enable flyckeck globally.
;;(add-hook 'after-init-hook #'global-flycheck-mode)

;; Enable company mode globally.
(add-hook 'after-init-hook 'global-company-mode)

;; Enable speedbar globally
;; (when window-system          ; start speedbar if we're using a window system
;;     (speedbar t))

;; Display the column number.
(setq column-number-mode t)

;; Enable projectile globally.
(projectile-global-mode)

;; Copy file path to clipboard.
(defun xah-copy-file-path (&optional *dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
Result is full path.
If `universal-argument' is called first, copy only the dir path.
URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
Version 2016-07-17"
  (interactive "P")
  (let ((-fpath
         (if (equal major-mode 'dired-mode)
             (expand-file-name default-directory)
           (if (null (buffer-file-name))
               (user-error "Current buffer is not associated with a file.")
             (buffer-file-name)))))
    (kill-new
     (if (null *dir-path-only-p)
         (progn
           (message "File path copied: 「%s」" -fpath)
           -fpath
           )
       (progn
         (message "Directory path copied: 「%s」" (file-name-directory -fpath))
         (file-name-directory -fpath))))))

;; Aspell-related configuration
(cond
 ((string-equal system-type "windows-nt")

  (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
  (setq ispell-program-name "aspell")
  (setq ispell-personal-dictionary "C:/Users/nadalesagutde/.ispell")))

;; Enable indent tools
(require 'indent-tools)
(global-set-key (kbd "C-c >") 'indent-tools-hydra/body)

(setq ring-bell-function 'ignore)

;; Enable the helm prjectile shortcut.
(global-set-key (kbd "C-c p h") 'helm-projectile)

;; Server:
;; Use emacs as a server. See manual section 31.3 (Using emacs as a
;; server). This is quite useful for having only a single instance of
;; the editor running.
(server-start)

;; TorXakis mode. For now we put this configuration here, till this mode is published on melpa.
(require 'torxakis-mode)
(defun my-prog-mode-hook ()
  "My programming mode hook."
  (setq tab-width 4))
(add-hook 'prog-mode-hook #'my-prog-mode-hook)

;; I don't use a secondary overlay for now.
(global-unset-key [M-mouse-1])
(global-unset-key [M-drag-mouse-1])
(global-unset-key [M-down-mouse-1])
(global-unset-key [M-mouse-3])
(global-unset-key [M-mouse-2])

;; Use xref-via helm.
(require 'helm-xref)
(setq xref-show-xrefs-function 'helm-xref-show-xrefs)

;; Delete trailing whitespace on save, globally.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Visually truncate long lines
(set-default 'truncate-lines t)

;; Source: https://zck.me/emacs-move-file
(defun move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive (list (expand-file-name
                      (if buffer-file-name
                          (read-file-name "Move file to: ")
                        (read-file-name "Move file to: "
                                        default-directory
                                        (expand-file-name (file-name-nondirectory (buffer-name))
                                                          default-directory))))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (expand-file-name (buffer-file-name))))
    (message "old file is %s and new file is %s"
             old-location
             new-location)
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))

;; Move files in place
(bind-key "C-x C-m" #'move-file)

(provide 'editor_common_config)
;;; editor_common_config.el ends here
