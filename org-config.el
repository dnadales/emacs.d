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
(defvar my/personal-notes-dir (concat org-directory "personal/notes/")
  "Directory of atomic personal notes, one file per note. Encrypted via git-crypt.")

;; NOTE: use a list in case I want to add more agenda files.
(setq org-agenda-files (list my/calendar-path))

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
