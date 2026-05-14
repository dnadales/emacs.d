;;; -*- lexical-binding: t; -*-
;;; programming-config.el
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(use-package vterm)

;;; programming-config.el ends here
