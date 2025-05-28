;;; programming-config.el
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(straight-use-package 'vterm)

;;; programming-config.el ends here
