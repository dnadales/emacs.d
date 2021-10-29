;;; org_config.el --- Org-mode configuration
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;; Settings for Emacs' org-mode.
;;;
;;; Code:
(require 'org)

;; This package seems to be outdated and it does not seem to work:
;; `org-store-link' will not add a git link when the cursor is at file version
;; controlled by git.
;;
;; (require 'org-git-link)

(setq org-directory "~/psys/")

(setq inbox-path (concat org-directory "inbox.org"))
(setq calendar-path (concat org-directory "calendar.org"))
(setq projects-path (concat org-directory "projects.org"))
(setq next-actions-path (concat org-directory "next-actions.org"))

;; Note that I use a list in case you want to add more agenda files.
(setq org-agenda-files (list calendar-path))

(setq org-default-notes-file calendar-path)

(define-key global-map "\C-cc" 'org-capture)

;; Global map to access the org-agenda commands.
(define-key global-map "\C-ca" 'org-agenda)

(define-key global-map "\C-cl" 'org-store-link)

;; This adds a ``CLOSED'' label to the TODO entry, which describes the
;; date and time in which the activity was marked as done.
(setq org-log-done t)

;; Capture templates
(setq org-capture-templates
      '(("t" "New Task" entry (file inbox-path)
         "* TODO %?\n  %U\n  %i\n")
        )
      )

;; See https://www.gnu.org/software/emacs/manual/html_node/org/Tracking-TODO-state-changes.html
;;
;; Meaning of symbols
;; @: leave a note
;; !: add a timestamp when entering that state
;;
(setq org-todo-keywords
       '((sequence "TODO(t!)" "WAITING(w@/!)" "STARTED(s!/!)" "DELEGATED(e@/!)" "|" "DONE(d!)" "CANCELED(c@/!)")))


;; To save the clock history accross Emacs sessions.
(setq org-clock-persist 'history)
(setq org-clock-into-drawer t)
(org-clock-persistence-insinuate)

;;
;; Agenda view customizations
;;
(setq  org-agenda-prefix-format
      '((agenda . "%?-12t%-11s")
        (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))
(setq org-agenda-todo-keyword-format "·")

(setq org-agenda-skip-scheduled-if-done t
      org-enforce-todo-dependencies t
      org-hide-leading-stars t
      org-log-into-drawer t
      org-odd-levels-only t)

;; Org-babel supported languages
(org-babel-do-load-languages
 'org-babel-load-languages '((haskell . t)
                             (dot . t))
 )
;;
;; Refile
;;
(setq org-refile-use-outline-path 'file) ; Use the file path as of the refiling
                                         ; targets. This makes it possible to
                                         ; refile nodes to the top-level.
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets (quote ((projects-path :level . 1)
                                 (next-actions-path :level . 1)
                                 (calendar-path :level . 1))))

;; Do not use the emacs theme to highlight code in the generated html.
(setq org-html-htmlize-output-type 'css)

(setq org-agenda-custom-commands
       `(;; match those tagged with :inbox:, are not scheduled, are not DONE.
         ("u" "[u]nscheduled tasks" tags "-SCHEDULED={.+}/!+TODO|+STARTED|+WAITING"))
       )

;; I tried org-wild-notifier and org-alert
;; they both error with
;;
;; Error (use-package): Failed to install org-wild-notifier: http://melpa.org/packages/log4e-20200420.745.el: Not found
;;

(use-package org-notifications
  :ensure t)
(require 'org-notifications)
(org-notifications-start)


;;; org_config.el ends here
