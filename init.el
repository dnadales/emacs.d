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
;; (package-initialize)

(provide 'init)

(load "~/.emacs.d/package_manager_config.el")
(load "~/.emacs.d/editor_common_config.el")
(load "~/.emacs.d/helm_config.el")
(load "~/.emacs.d/org_config.el")
(load "~/.emacs.d/windows_management_config.el")
;; (load "~/.emacs.d/haskell_config.el")
(load "~/.emacs.d/haskell_lsp.el")
(load "~/.emacs.d/latex_config.el")
;;(load "~/.emacs.d/haskell_intero_config.el")
;;(load "~/.emacs.d/scala_config.el")

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(confirm-kill-emacs 'y-or-n-p)
 '(custom-safe-themes
   '("ae88c445c558b7632fc2d72b7d4b8dfb9427ac06aa82faab8d760fff8b8f243c" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "df01ad8d956b9ea15ca75adbb012f99d2470f33c7b383a8be65697239086672e" "5a04c3d580e08f5fc8b3ead2ed66e2f0e5d93643542eec414f0836b971806ba9" "13fa7a304bd53aa4c0beec4c25c4f811de499bce9deb326798265ed0015b3b78" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "274fa62b00d732d093fc3f120aca1b31a6bb484492f31081c1814a858e25c72e" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "0d456bc74e0ffa4bf5b69b0b54dac5104512c324199e96fc9f3a1db10dfa31f3" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "b46ee2c193e350d07529fcd50948ca54ad3b38446dcbd9b28d0378792db5c088" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "b9a06c75084a7744b8a38cb48bc987de10d68f0317697ccbd894b2d0aca06d2b" "f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" "5cd0afd0ca01648e1fff95a7a7f8abec925bd654915153fb39ee8e72a8b56a1f" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "bfdcbf0d33f3376a956707e746d10f3ef2d8d9caa1c214361c9c08f00a1c8409" "83faf27892c7119f6016e3609f346d3dae3516dede8fd8a5940373d98f615b4e" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "bb4733b81d2c2b5cdec9d89c111ef28a0a8462a167d411ced00a77cfd858def1" default))
 '(haskell-indentation-electric-flag t)
 '(haskell-interactive-popup-errors nil)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-hoogle-imports t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type 'stack-ghci)
 '(haskell-stylish-on-save nil)
 '(haskell-tags-on-save t)
 '(js-indent-level 2)
 '(kill-ring-max 10000)
 '(line-spacing 0.3)
 '(lsp-haskell-server-path "haskell-language-server")
 '(lsp-keymap-prefix "M-s-l")
 '(nix-indent-function 'nix-indent-line)
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-window-setup 'current-window)
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(org-indirect-buffer-display 'current-window)
 '(org-src-fontify-natively t)
 '(org-src-window-setup 'current-window)
 '(package-selected-packages
   '(lsp-haskell lsp-mode spacemacs-theme flatui-dark-theme flatui-theme espresso-theme idea-darkula-theme speed-type magit editorconfig markdown-toc alect-themes distinguished-theme dracula-theme edit-indirect moe-theme mmm-mode auctex helm-xref elm-mode flycheck-demjsonlint json-mode vmd-mode company-nixos-options nix-buffer nix-sandbox helm-nixos-options nix-mode intero htmlize paredit flycheck-haskell markdown-mode+ protobuf-mode terraform-mode color-theme-solarized solarized-theme anti-zenburn-theme hc-zenburn-theme labburn-theme zenburn-theme hydra indent-tools groovy-mode docker dockerfile-mode cypher-mode ox-reveal yaml-mode flymd gh-md markdown-mode buffer-move use-package helm-projectile haskell-snippets haskell-mode flycheck ensime))
 '(projectile-use-git-grep t)
 '(safe-local-variable-values
   '((haskell-mode-stylish-haskell-path . "stylish-haskell")
     (haskell-mode-stylish-haskell-path . "cls-stylish-haskell")
     (haskell-mode-stylish-haskell-path . stylish-haskell)
     (haskell-stylish-on-save . t)
     (intero-targets . "stack_linux.yaml")))
 '(smie-indent-basic 10))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
