;;; ui-config.el
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;; Tweaks that concern the user interface exclusively.
;;;
;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface cleaning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-message t)
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar
(setq visible-bell t)       ; Disable the audible bell

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme setting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(straight-use-package 'dracula-theme)
(load-theme 'dracula t)

;; (straight-use-package 'solarized-theme)
;; (load-theme 'solarized-light t)

; (load-theme 'modus-operandi)

;; Disable underline in language notes. This affects lsp rendering.
;;
;; I found this setting by running:
;;
;;     M-x customize-face RET flymake-note RET
;;
(custom-theme-set-faces 'user
			`(modus-themes-lang-note    ((t (:underline nil))) t)
			`(modus-themes-lang-warning ((t (:underline nil))) t))

;; https://www.jetbrains.com/lp/mono/#how-to-install
;;(set-frame-font "JetBrains Mono-12")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pulsar configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(straight-use-package 'pulsar)
(require 'pulsar)
(setq pulsar-pulse t)
(setq pulsar-delay 0.055)
(setq pulsar-iterations 10)
(setq pulsar-face 'pulsar-magenta)
(setq pulsar-highlight-face 'pulsar-yellow)
(pulsar-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq column-number-mode t)

;; Moody
(straight-use-package 'moody)
(setq x-underline-at-descent-line t)
(moody-replace-mode-line-buffer-identification)
(moody-replace-vc-mode)
(moody-replace-eldoc-minibuffer-message-function)

;; Minions: minimize the minor modes shown in the modeline.
(straight-use-package 'minions)
(minions-mode 1)

;;; ui-config.el ends here
