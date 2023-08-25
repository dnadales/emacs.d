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
(load-theme 'modus-vivendi)
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
(set-frame-font "JetBrains Mono-15")

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

;; Doom modeline requeres nerd-icons https://github.com/rainstormstudio/nerd-icons.el
(straight-use-package 'nerd-icons)
;; run M-x nerd-icons-install-fonts
(straight-use-package 'doom-modeline)
(doom-modeline-mode 1)
;; See https://github.com/seagle0128/doom-modeline for additional customization
(setq doom-modeline-buffer-file-name-style 'buffer-name)
(setq doom-modeline-buffer-encoding nil)

;;; ui-config.el ends here
