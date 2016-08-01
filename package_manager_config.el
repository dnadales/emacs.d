;;; package_manager_config.el  --- Package management configuration.
;;;
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;; Configuration related to package management.
;;;
;;; Code:

(require 'package)

(setq
 use-package-always-ensure t
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; Path to my own packages.
(add-to-list 'load-path "~/.emacs.d/elisp/")

(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Required packages
(use-package ensime
             :commands ensime ensime-mode)
(use-package flycheck)
(use-package haskell-mode)
(use-package intero)
(use-package projectile)
(use-package helm-projectile)
;;; package_manager_config.el ends here

