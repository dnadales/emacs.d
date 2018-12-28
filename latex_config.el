;;; latex_config.el --- Configuration for edditing LaTeX files.
;;;
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;;   Credits: http://funloop.org/post/2015-07-28-literate-haskell-mmm-mode.html
;;;
;;; Code:
(require 'reftex)
(require 'mmm-mode)

; Remove the hard-coded 'literate-haskell-mode' activation for `.lhs' files that
; haskell-mode comes with. In exchange, enable LaTeX mode whenever we open up a
; `.lhs' file. Using mmm-mode, we will activate `haskell-mode' in the code
; sections.
(setq auto-mode-alist
  (remove
    (rassoc 'literate-haskell-mode auto-mode-alist) auto-mode-alist))
(add-to-list 'auto-mode-alist '("\\.lhs$" . latex-mode))

; Literate Haskell - mmm-mode. Adopted from
; https://wiki.haskell.org/Literate_programming#Multi-mode_support_in_Emacs

(mmm-add-classes
  '((literate-haskell-latex
    :submode haskell-mode
    :front "^\\\\begin{code}\n"
    :back "^\\\\end{code}"
  )))

; Re-fontify sub-mode portions when idle. The manual command for this is
; `mmm-parse-buffer'. If you don't do this, then syntax highlighting won't work
; for new regions of Haskell code in the \begin{code}...\end{code} blocks.
(setq mmm-parse-when-idle 't)


;; Turn on RefTeX with AucTex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; RefTeX configuration
(setq-default TeX-master nil) ; Query for master file.
(setq TeX-parse-self t) ; Enable parse on load.
(setq TeX-auto-save t) ; Enable parse on save.

;;; latex_config.el ends here
