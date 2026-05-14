;;; -*- lexical-binding: t; -*-
;;; writing-config.el
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;; Spell-checking and text editing settings.
;;;
;;; Code:

(setq ispell-program-name "hunspell")

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Semantic selection expansion: repeated C-= widens the selection
;; from word -> sentence -> paragraph -> function body, etc.
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;; writing-config.el ends here
