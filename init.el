;;; init.el --- Emacs configuration file
;;; Author: Damian Nadales
;;;
;;; Commentary:
;;; My Emacs configuration.
;;;
;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(provide 'init)

(load "~/.emacs.d/package_manager_config.el")
(load "~/.emacs.d/editor_common_config.el")
(load "~/.emacs.d/helm_config.el")
(load "~/.emacs.d/org_config.el")
(load "~/.emacs.d/windows_management_config.el")
(load "~/.emacs.d/haskell_config.el")
;;(load "~/.emacs.d/haskell_intero_config.el")
(load "~/.emacs.d/scala_config.el")

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes
   (quote
    ("f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" "5cd0afd0ca01648e1fff95a7a7f8abec925bd654915153fb39ee8e72a8b56a1f" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "bfdcbf0d33f3376a956707e746d10f3ef2d8d9caa1c214361c9c08f00a1c8409" "83faf27892c7119f6016e3609f346d3dae3516dede8fd8a5940373d98f615b4e" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "bb4733b81d2c2b5cdec9d89c111ef28a0a8462a167d411ced00a77cfd858def1" default)))
 '(haskell-indentation-electric-flag t)
 '(haskell-indentation-layout-offset 4)
 '(haskell-indentation-left-offset 4)
 '(haskell-indentation-starter-offset 4)
 '(haskell-indentation-where-post-offset 2)
 '(haskell-indentation-where-pre-offset 4)
 '(haskell-interactive-popup-errors nil)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote stack-ghci))
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(js-indent-level 2)
 '(kill-ring-max 10000)
 '(org-export-backends (quote (ascii html icalendar latex md odt)))
 '(package-selected-packages
   (quote
    (helm-xref elm-mode flycheck-demjsonlint json-mode vmd-mode company-nixos-options nix-buffer nix-sandbox helm-nixos-options nix-mode intero htmlize paredit flycheck-haskell markdown-mode+ protobuf-mode terraform-mode color-theme-solarized solarized-theme anti-zenburn-theme hc-zenburn-theme labburn-theme zenburn-theme hydra indent-tools groovy-mode docker dockerfile-mode cypher-mode ox-reveal yaml-mode flymd gh-md markdown-mode buffer-move use-package helm-projectile haskell-snippets haskell-mode flycheck ensime)))
 '(safe-local-variable-values
   (quote
    ((intero-targets . "blabla.yaml")
     (intero-targets . "stack_pepe.yaml")
     (intero-targets . "stack_linux.yaml")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
