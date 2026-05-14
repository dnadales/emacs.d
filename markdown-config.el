;;; -*- lexical-binding: t; -*-
;;; markdown-config.el --- Configuration for Markdown support
;;;
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:

(with-eval-after-load 'markdown-mode
  (setq markdown-fontify-code-blocks-natively t)
  (add-to-list 'markdown-code-lang-modes '("quint" . quint-mode)))

(add-hook 'markdown-mode-hook 'markdown-toggle-markup-hiding)
(add-hook 'markdown-mode-hook 'visual-line-mode)

;;; markdown-config.el ends here
