;;; org_config.el --- Org-mode configuration
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;; Settings for Emacs' org-mode.
;;;
;;; Code:
(require 'org)

(setq org-directory "~/CloudStation/org/")

(setq agenda-path (concat org-directory "agenda.org"))

;; Note that I use a list in case you want to add more agenda files.
(setq org-agenda-files (list agenda-path))

(setq org-default-notes-file agenda-path)

(define-key global-map "\C-cc" 'org-capture)

;; Global map to access the org-agenda commands.
(define-key global-map "\C-ca" 'org-agenda)

;; This adds a ``CLOSED'' label to the TODO entry, which describes the
;; date and time in which the activity was marked as done.
(setq org-log-done t)

;; Capture templates
(setq org-capture-templates
      '(("t" "New Task" entry (file+headline agenda-path "Inbox")
         "* TODO %?\n  %T\n  %i\n  %a")
        )
      )

;; See https://www.gnu.org/software/emacs/manual/html_node/org/Tracking-TODO-state-changes.html
(setq org-todo-keywords
       '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!/!)" "|" "DONE(d!)" "CANCELED(c@)")))


;; To save the clock history accross Emacs sessions.
(setq org-clock-persist 'history)
(setq org-clock-into-drawer t)
(org-clock-persistence-insinuate)

;; The following hooks allow to clock in and out based on the state changes.
;; Taken from: http://sachachua.com/blog/2007/12/a-day-in-a-life-with-org/

(defun damian/org-clock-in-if-starting ()
  "Clock in when the task is marked STARTED."
  (when
      (and (string= org-state "STARTED")
           (not (string= org-last-state org-state)))
    (org-clock-in)))

(add-hook 'org-after-todo-state-change-hook
	  'damian/org-clock-in-if-starting)

(defadvice org-clock-in (after damian activate)
  "Set this task's status to 'STARTED'."
  (org-todo "STARTED"))

(defun damian/org-clock-out-if-not-done ()
  "Clock out when the task is marked as not done (WAITING or TODO) ."
  (when 
      (and
       (org-clock-is-active)
       (or (string= org-state "WAITING")
             (string= org-state "TODO"))
       (not (string= org-last-state org-state)))
    (message "Trying to clock out...")
    (org-clock-out)
    (message "Clock out done...")))

(add-hook 'org-after-todo-state-change-hook
	  'damian/org-clock-out-if-not-done)

(setq org-agenda-skip-scheduled-if-done t
      org-enforce-todo-dependencies t
      org-hide-leading-stars t
      org-log-into-drawer t
      org-odd-levels-only t)

;; Load scala mode
(org-babel-do-load-languages
 'org-babel-load-languages '(
                             (scala . t)
                             (haskell . t)
                             )
 )

;; Org-reveal
(setq org-reveal-root "file:///home/damian/opt/revealjs/reveal.js")


;;; org_config.el ends here
