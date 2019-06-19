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
(load "~/.emacs.d/latex_config.el")
;;(load "~/.emacs.d/haskell_intero_config.el")
;;(load "~/.emacs.d/scala_config.el")

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(confirm-kill-emacs (quote y-or-n-p))
 '(custom-safe-themes
   (quote
    ("a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "0d456bc74e0ffa4bf5b69b0b54dac5104512c324199e96fc9f3a1db10dfa31f3" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "b46ee2c193e350d07529fcd50948ca54ad3b38446dcbd9b28d0378792db5c088" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "b9a06c75084a7744b8a38cb48bc987de10d68f0317697ccbd894b2d0aca06d2b" "f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" "5cd0afd0ca01648e1fff95a7a7f8abec925bd654915153fb39ee8e72a8b56a1f" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "bfdcbf0d33f3376a956707e746d10f3ef2d8d9caa1c214361c9c08f00a1c8409" "83faf27892c7119f6016e3609f346d3dae3516dede8fd8a5940373d98f615b4e" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "bb4733b81d2c2b5cdec9d89c111ef28a0a8462a167d411ced00a77cfd858def1" default)))
 '(haskell-indentation-electric-flag t)
 '(haskell-interactive-popup-errors nil)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote stack-ghci))
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save t)
 '(js-indent-level 2)
 '(kill-ring-max 10000)
 '(org-export-backends (quote (ascii html icalendar latex md odt)))
 '(package-selected-packages
   (quote
    (editorconfig markdown-toc alect-themes distinguished-theme dracula-theme edit-indirect moe-theme mmm-mode auctex helm-xref elm-mode flycheck-demjsonlint json-mode vmd-mode company-nixos-options nix-buffer nix-sandbox helm-nixos-options nix-mode intero htmlize paredit flycheck-haskell markdown-mode+ protobuf-mode terraform-mode color-theme-solarized solarized-theme anti-zenburn-theme hc-zenburn-theme labburn-theme zenburn-theme hydra indent-tools groovy-mode docker dockerfile-mode cypher-mode ox-reveal yaml-mode flymd gh-md markdown-mode buffer-move use-package helm-projectile haskell-snippets haskell-mode flycheck ensime)))
 '(safe-local-variable-values
   (quote
    ((haskell-mode-stylish-haskell-path . "stylish-haskell")
     (haskell-mode-stylish-haskell-path . "cls-stylish-haskell")
     (haskell-mode-stylish-haskell-path . stylish-haskell)
     (haskell-stylish-on-save . t)
     (intero-targets . "stack_linux.yaml")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
