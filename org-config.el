;;; -*- lexical-binding: t; -*-
;;; org-config.el
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:

(setq org-directory "~/psys/")
(setq org-export-backends '(ascii html icalendar latex md odt))

(defvar my/inbox-path (concat org-directory "inbox.org"))
(defvar my/calendar-path (concat org-directory "calendar.org"))
(defvar my/projects-path (concat org-directory "projects.org"))
(defvar my/next-actions-path (concat org-directory "next-actions.org"))
(defvar my/maybe-someday-path (concat org-directory "maybe-someday.org"))
(defvar my/notes-path (concat org-directory "notes/notes.org"))
(defvar my/personal-journal-path (concat org-directory "personal/journal.org"))
(defvar my/personal-calendar-path (concat org-directory "personal/calendar.org")
  "Encrypted (git-crypt) calendar of sensitive dated items, e.g. health follow-ups.")
(defvar my/personal-notes-dir (concat org-directory "personal/notes/")
  "Directory of atomic personal notes, one file per note. Encrypted via git-crypt.")

;; NOTE: use a list in case I want to add more agenda files.
(setq org-agenda-files (list my/calendar-path my/next-actions-path my/personal-calendar-path))

(setq org-default-notes-file my/calendar-path)

;;
;; Org capture configuration
;;
(define-key global-map "\C-cc" 'org-capture)

;; Personal notes: one encrypted file per note under personal/notes/.
;; Capture (C-c c N) opens a new file under a temporary timestamp name and drops
;; you straight in, no title prompt. You type the #+title: and the body; on
;; finalize `my/personal-note-rename' renames the file to <timestamp>-<slug>.org.
(defvar my/personal-note-file nil
  "Path of the personal note being captured, or nil when not capturing.")

(defun my/slugify (title)
  "Turn TITLE into a filename-safe slug.
Lowercase it, collapse runs of non-alphanumerics to single hyphens, and trim
leading and trailing hyphens. Return \"untitled\" when nothing is left."
  (let* ((down (downcase title))
         (hyphenated (replace-regexp-in-string "[^[:alnum:]]+" "-" down))
         (trimmed (replace-regexp-in-string "\\`-+\\|-+\\'" "" hyphenated)))
    (if (string= trimmed "") "untitled" trimmed)))

(defun my/personal-note-goto ()
  "Create a new personal note file under a temporary timestamp name.
Used as the `org-capture' target for the \"N\" template. The file is renamed to
include the title slug by `my/personal-note-rename' on finalize."
  (let ((file (expand-file-name
               (format-time-string "%Y%m%d%H%M%S.org")
               my/personal-notes-dir)))
    (setq my/personal-note-file file)
    (make-directory my/personal-notes-dir t)
    (set-buffer (org-capture-target-buffer file))
    (goto-char (point-max))))

(defun my/org-file-title (file)
  "Return the trimmed #+title: keyword value of org FILE, or nil if absent."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+title:[ \t]*\\(.*\\)$" nil t)
      (string-trim (match-string 1)))))

(defun my/personal-note-rename ()
  "Rename a just-captured personal note to <timestamp>-<slug>.org.
Reads the #+title: from the saved file. A missing or empty title leaves the
plain timestamp name. Runs from `org-capture-after-finalize-hook'."
  (when my/personal-note-file
    (let ((file my/personal-note-file))
      (setq my/personal-note-file nil)      ; one shot, also clears on abort
      (when (file-exists-p file)
        (let* ((title (my/org-file-title file))
               (slug (and title (my/slugify title))))
          (when (and slug (not (string= slug "untitled")))
            (let ((new (expand-file-name
                        (concat (file-name-base file) "-" slug ".org")
                        my/personal-notes-dir)))
              (unless (or (string= file new) (file-exists-p new))
                (let ((buf (find-buffer-visiting file)))
                  (rename-file file new)
                  (when buf
                    (with-current-buffer buf
                      (set-visited-file-name new t t)
                      (set-buffer-modified-p nil))))))))))))

(add-hook 'org-capture-after-finalize-hook #'my/personal-note-rename)

;; Capture templates
(setq org-capture-templates
      '(("t" "New Task" entry (file my/inbox-path)
         "* TODO %?\n  %U\n  %i\n")
        ("n" "Note" entry (file my/notes-path)
         "* %? %^g\n  %<%Y-%m-%d %a %H:%M:%S>\n  %i\n")
        ("J" "Personal journal" entry (file my/personal-journal-path)
         "* %U %?\n")
        ("N" "Personal note" plain (function my/personal-note-goto)
         "#+title: %?\n#+filetags: :personal:\n#+date: %U\n\n"
         :unnarrowed t :empty-lines 0)))

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

;; Two agenda views split by tag. Work items carry :work: (calendar.org work
;; habits + next-actions.org via #+FILETAGS), household items carry :home:.
;; Each view = a tag-filtered timeline + a list of open tagged TODOs, so
;; unscheduled work tasks still appear.
;;
;; The tag-filter-preset must sit at the command level (the 4th element), not on
;; the agenda block: a composite command prepares and finalizes the buffer once,
;; and only the command-level preset reaches that single filter pass. The
;; tags-todo blocks filter via their own match string, so the command-level
;; preset is redundant for them but harmless (their entries already match).
(setq org-agenda-custom-commands
      '(("w" "Work agenda"
         ((agenda "")
          (tags-todo "+work" ((org-agenda-overriding-header "Open work tasks"))))
         ((org-agenda-tag-filter-preset '("+work"))))
        ("h" "Home agenda"
         ((agenda "")
          (tags-todo "+home" ((org-agenda-overriding-header "Open home tasks"))))
         ((org-agenda-tag-filter-preset '("+home"))))))

;; Habit consistency graph. For every :STYLE: habit entry the agenda draws a
;; strip showing the last weeks: a filled glyph for days the habit was done on
;; time, empty for missed. Require org-habit directly (it pulls in org and turns
;; on the graph). Don't touch `org-modules' here: this file runs before org is
;; loaded, so that variable is still void and `add-to-list' on it would error.
;; Tuning knobs: org-habit-graph-column (where the graph starts, default 40),
;; org-habit-preceding-days / org-habit-following-days (window width),
;; org-habit-show-habits-only-for-today (default t: in the weekly view show each
;; habit once, on today, instead of repeating it every day).
(require 'org-habit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Habit / recurring-task adherence report
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `my/habit-report' (C-c h) reads the DONE entries logged in each recurring
;; item's LOGBOOK and reports, per item, how many of the last N periods you
;; actually did it. N and the period length come from the item's own repeater:
;; a .+1d habit is scored over the last 30 days, a ++1w task over 12 weeks, a
;; .+1m chore over 12 months. This is the tracker for weekly and slower
;; cadences, where the org-habit graph (built for daily habits over ~3 weeks) is
;; too coarse. Month and year lengths are approximated (30 / 365 days).

(defvar my/habit-report-windows '(("d" . 30) ("w" . 12) ("m" . 12) ("y" . 5))
  "How many periods to look back, keyed by the repeat unit.")

(defun my/habit--period-days (count unit)
  "Length in days of COUNT repeats of UNIT (d/w/m/y); months and years approx."
  (* count (pcase unit ("d" 1) ("w" 7) ("m" 30) ("y" 365) (_ 1))))

(defun my/habit--window (unit)
  "Number of periods to score for repeat UNIT."
  (or (cdr (assoc unit my/habit-report-windows)) 12))

(defun my/habit--done-times ()
  "Emacs times of the DONE state-changes logged in the current subtree."
  (let ((end (save-excursion (org-end-of-subtree t t)))
        times)
    (save-excursion
      (while (re-search-forward "State \"DONE\".*?\\[\\([^]]+\\)\\]" end t)
        (push (org-time-string-to-time (match-string 1)) times)))
    times))

(defun my/habit-report ()
  "Report adherence for every recurring entry in the agenda files.
For each entry with a repeater, count over the last N periods (N and the period
length taken from the repeater) how many periods had at least one DONE, and show
the ratio and percentage, worst first."
  (interactive)
  (let (rows)
    (org-map-entries
     (lambda ()
       (let ((repeat (org-get-repeat)))
         (when (and repeat (string-match "\\([0-9]+\\)\\([hdwmy]\\)" repeat))
           (let* ((count (string-to-number (match-string 1 repeat)))
                  (unit (let ((u (match-string 2 repeat))) (if (string= u "h") "d" u)))
                  (pd (my/habit--period-days count unit))
                  (win (my/habit--window unit))
                  (times (my/habit--done-times))
                  (now (current-time))
                  (hits 0))
             (dotimes (i win)
               (let ((hi (time-subtract now (days-to-time (* i pd))))
                     (lo (time-subtract now (days-to-time (* (1+ i) pd)))))
                 (when (seq-some (lambda (tm) (and (time-less-p lo tm)
                                                   (not (time-less-p hi tm))))
                                 times)
                   (setq hits (1+ hits)))))
             (push (list (org-get-heading t t t t)
                         (mapconcat #'identity (org-get-tags) ":")
                         repeat hits win
                         (round (* 100.0 (/ hits (float win)))))
                   rows)))))
     nil 'agenda)
    (setq rows (sort rows (lambda (a b) (< (nth 5 a) (nth 5 b)))))
    (with-current-buffer (get-buffer-create "*Habit report*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Recurring-task adherence  (as of %s)\n\n"
                        (format-time-string "%Y-%m-%d")))
        (insert (format "%-30s %-14s %-7s %-9s %s\n"
                        "Entry" "Tags" "Repeat" "Done" "Adherence"))
        (insert (make-string 74 ?-) "\n")
        (dolist (r rows)
          (insert (format "%-30s %-14s %-7s %-9s %3d%%\n"
                          (truncate-string-to-width (nth 0 r) 30)
                          (nth 1 r) (nth 2 r)
                          (format "%d/%d" (nth 3 r) (nth 4 r))
                          (nth 5 r)))))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(define-key global-map "\C-ch" 'my/habit-report)


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
