;;; markdown-config.el --- Configuration for Markdown support
;;;
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:

(add-hook 'markdown-mode-hook 'visual-fill-column-mode)
(add-hook 'markdown-mode-hook 'markdown-toggle-markup-hiding)

;;; markdown-config.el ends here
