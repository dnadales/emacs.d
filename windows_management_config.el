;;; windows_management_config.el
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

;; Install package ``buffer-move''
(require 'buffer-move)

(global-set-key (kbd "C-c M-j") 'buf-move-left)
(global-set-key (kbd "C-c M-l") 'buf-move-right)
(global-set-key (kbd "C-c M-i") 'buf-move-up)
(global-set-key (kbd "C-c M-k") 'buf-move-down)


;; windows_management_config.el ends here
