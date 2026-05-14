;;; -*- lexical-binding: t; -*-
;;; windows-management-config.el
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:

(when (fboundp 'winner-mode)
  (winner-mode 1))

(global-set-key (kbd "C-c n") 'windmove-left)
(global-set-key (kbd "C-c i") 'windmove-right)
(global-set-key (kbd "C-c u") 'windmove-up)
(global-set-key (kbd "C-c e") 'windmove-down)

(use-package buffer-move
  :bind (("C-c M-n" . buf-move-left)
         ("C-c M-i" . buf-move-right)
         ("C-c M-u" . buf-move-up)
         ("C-c M-e" . buf-move-down)))

;; Jump to any visible text in 2-3 keystrokes.
(use-package avy
  :bind (("C-c j" . avy-goto-char-timer)
         ("M-g w" . avy-goto-word-1)))

;;; windows-management-config.el ends here
