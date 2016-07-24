;;; windows_management_config.el
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;;
;;; Code:

(when (fboundp 'winner-mode)
  (winner-mode 1))

(global-set-key (kbd "C-c j") 'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c i") 'windmove-up)
(global-set-key (kbd "C-c k") 'windmove-down)

;; Install package ``buffer-move''
(require 'buffer-move)

(global-set-key (kbd "C-c M-j") 'buf-move-left)
(global-set-key (kbd "C-c M-l") 'buf-move-right)
(global-set-key (kbd "C-c M-i") 'buf-move-up)
(global-set-key (kbd "C-c M-k") 'buf-move-down)


;; windows_management_config.el ends here
