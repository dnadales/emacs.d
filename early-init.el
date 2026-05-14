;;; -*- lexical-binding: t; -*-
;;; early-init.el --- Emacs early configuration
;;; Author: Damian Nadales
;;;
;;; Code:

;; https://github.com/radian-software/straight.el#getting-started
(setq package-enable-at-startup nil)

;; Disable UI chrome early to avoid flicker on startup.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;;; early-init.el ends here
