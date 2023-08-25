;;; org-config.el
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:
(straight-use-package 'org)
(require 'org)

(setq org-directory "~/psys/")

(setq inbox-path (concat org-directory "inbox.org"))
(setq calendar-path (concat org-directory "calendar.org"))
(setq projects-path (concat org-directory "projects.org"))
(setq next-actions-path (concat org-directory "next-actions.org"))

;; NOTE: use a list in case I want to add more agenda files.
(setq org-agenda-files (list calendar-path))

(setq org-default-notes-file calendar-path)

;;
;; Org capture configuration
;;
(define-key global-map "\C-cc" 'org-capture)

;; Capture templates
(setq org-capture-templates
      '(("t" "New Task" entry (file inbox-path)
         "* TODO %?\n  %U\n  %i\n")
        )
      )

;;
;; Link management
;;
(define-key global-map "\C-cl" 'org-store-link)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UI settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Headings
(setq org-hide-leading-stars t)
(setq org-odd-levels-only t)

;; Headings appearance
(custom-theme-set-faces 'user
                        `(org-level-1 ((t (:foreground "dark turquoise"
					   :height     1.0))))
			`(org-level-2 ((t (:foreground "pale turquoise"))))
			`(org-level-3 ((t (:foreground "lemon chiffon"))))
			`(org-level-4 ((t (:foreground "white smoke"))))
			`(org-level-5 ((t (:foreground "white smoke"))))
			`(org-level-6 ((t (:foreground "white smoke")))))

;; Org bullets
(straight-use-package 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Make text lines that are not headlines prefixed with virtual spaces
;; to vertically align with the headline text.
(setq org-startup-indented t) ;; https://orgmode.org/manual/Org-Indent-Mode.html

;; Hide the emphasis markup (eg. /.../ for italics, *...* for bold, etc.):
(setq org-hide-emphasis-markers t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keywords settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; See https://www.gnu.org/software/emacs/manual/html_node/org/Tracking-TODO-state-changes.html
;;
;; Meaning of symbols
;; @: leave a note
;; !: add a timestamp when entering that state
;;
(setq org-todo-keywords
      '((sequence "TODO(t!)"
		  "WAITING(w@/!)"
		  "STARTED(s!/!)"
		  "DELEGATED(e@/!)"
		  "|"
		  "DONE(d!)"
		  "CANCELED(c@/!)")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
	("WAITING" . "orange")
	("STARTED" . "yellow")
	("DONE" . (:foreground "green4" :weight bold))
        ("CANCELED" . (:foreground "blue" :weight bold))))

(custom-set-faces
 '(org-headline-done
   ((((class color)
      (min-colors 16)
      (background dark))
     (:strike-through nil :foreground "rosy brown")))))

;; This adds a ``CLOSED'' label to the TODO entry, which describes the
;; date and time in which the activity was marked as done.
(setq org-log-done t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-log-into-drawer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agenda view settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Global map to access the org-agenda commands.
(define-key global-map "\C-ca" 'org-agenda)
(setq org-agenda-skip-scheduled-if-done t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Babel settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(org-babel-do-load-languages
 'org-babel-load-languages '((haskell . t)
                             (dot . t))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Refile settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-refile-use-outline-path 'file) ; Use the file path as of the refiling
                                         ; targets. This makes it possible to
                                         ; refile nodes to the top-level.
(setq org-reverse-note-order t) ; Refile at the top of the file or node.
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets (quote ((projects-path :level . 1)
                                 (next-actions-path :level . 1)
                                 (calendar-path :level . 1))))


;;; org-config.el ends here
