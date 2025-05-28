;;; markdown-config.el --- Configuration for Markdown support
;;;
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:

(add-hook 'markdown-mode-hook 'visual-fill-column-mode)
(add-hook 'markdown-mode-hook 'markdown-toggle-markup-hiding)
(add-hook 'markdown-mode-hook 'visual-line-mode)

;;; markdown-config.el ends here
