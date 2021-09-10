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

;; Refile
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

;; Do not use the emacs theme to highlight code in the generated html.
(setq org-html-htmlize-output-type 'css)

(setq org-agenda-custom-commands
       `(;; match those tagged with :inbox:, are not scheduled, are not DONE.
         ("u" "[u]nscheduled tasks" tags "-SCHEDULED={.+}/!+TODO|+STARTED|+WAITING"))
       )

;;; org_config.el ends here
