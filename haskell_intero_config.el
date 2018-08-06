;;; haskell_intero_config.el --- Configuration for haskell-mode, using intero.
;;;
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:
;; If you don't have MELPA in your package archives:
(add-hook 'haskell-mode-hook 'intero-mode)

;; Improved flycheck
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;;; haskell_intero_config.el ends here
