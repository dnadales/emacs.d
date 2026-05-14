;;; -*- lexical-binding: t; -*-
;;; org-config.el
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:

(setq org-directory "~/psys/")

(defvar my/inbox-path (concat org-directory "inbox.org"))
(defvar my/calendar-path (concat org-directory "calendar.org"))
(defvar my/projects-path (concat org-directory "projects.org"))
(defvar my/next-actions-path (concat org-directory "next-actions.org"))
(defvar my/maybe-someday-path (concat org-directory "maybe-someday.org"))
(defvar my/notes-path (concat org-directory "notes/notes.org"))

;; NOTE: use a list in case I want to add more agenda files.
(setq org-agenda-files (list my/calendar-path))

(setq org-default-notes-file my/calendar-path)

;;
;; Org capture configuration
;;
(define-key global-map "\C-cc" 'org-capture)

;; Capture templates
(setq org-capture-templates
      '(("t" "New Task" entry (file my/inbox-path)
         "* TODO %?\n  %U\n  %i\n")
        ("n" "Note" entry (file my/notes-path)
         "* %? %^g\n  %<%Y-%m-%d %a %H:%M:%S>\n  %i\n")))

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

;; Org superstar (maintained successor to org-bullets)
(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

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
      '(("TODO" . "red")
	("WAITING" . "orange")
	("STARTED" . "yellow")
	("DONE" . (:foreground "green4" :weight bold))
        ("CANCELED" . (:foreground "brown" :weight bold))))

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
                             (dot . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Refile settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-refile-use-outline-path 'file) ; Use the file path as of the refiling
                                         ; targets. This makes it possible to
                                         ; refile nodes to the top-level.
(setq org-reverse-note-order t) ; Refile at the top of the file or node.
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets (quote ((my/projects-path      :level . 1)
                                 (my/next-actions-path  :level . 1)
                                 (my/calendar-path      :level . 1)
				 (my/maybe-someday-path :level . 1))))


;;; org-config.el ends here
