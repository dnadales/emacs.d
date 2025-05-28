(straight-use-package 'rust-mode)
(straight-use-package 'racer) ; Optional, but highly recommended for code completion, etc.
(straight-use-package 'flycheck) ; Optional, but recommended for live error checking.

(require 'rust-mode)

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t))

;; Make sure we indent Rust files with spaces, as recommended in the style guide.
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;; Format on save via rustfmt
(setq rust-format-on-save t) ; Bound by default to C-c C-f

;; LSP config
(add-hook 'rust-mode-hook #'lsp)
