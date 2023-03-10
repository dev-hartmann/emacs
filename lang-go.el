(use-package go-mode
 :general
 (general-define-key
   :states '(normal visual motion)
   :keymaps evil-collection-go-mode-maps
    "g d" '(lsp-find-definition :wk "goto definition")))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Start LSP Mode and YASnippet mode
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)
