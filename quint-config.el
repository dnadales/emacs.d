;;; quint-config.el
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:

(straight-use-package
 '(quint-mode :type git
              :host github
              :repo "informalsystems/quint"
              :files ("editor-plugins/emacs/quint-mode.el")))

(require 'quint-mode)
(add-to-list 'auto-mode-alist '("\\.qnt\\'" . quint-mode))

;;; quint-config.el ends here
