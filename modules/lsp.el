;; lsp.el --- LSP integration -*- lexical-binding: t -*-

;; Filename: lsp.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(use-package lsp-mode
  :commands
  (lsp lsp-deferred)
  :hook
  ((lsp-mode . (lambda () (setq-local evil-lookup-func #'lsp-describe-thing-at-point)))
   (lsp-mode . lsp-enable-which-key-integration))
  :general
  (general-define-key
   :states 'normal
   "g l" '(:ignore t :which-key "code")
   "g l l" '(:keymap lsp-command-map :wk "lsp")
   "g l a" '(lsp-execute-code-action :wk "code action")
   "g l r" '(lsp-rename :wk "rename"))
  :init
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-completion-provider :none)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-before-save-edits t)
  (setq lsp-headerline-breadcrumb-enable nil))

(defun corfu-lsp-setup ()
  (setq-local completion-styles '(orderless)
              completion-category-defaults nil))
(add-hook 'lsp-mode-hook #'corfu-lsp-setup)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config (setq lsp-ui-doc-enable nil)
  :general
  (general-define-key
   :states '(normal visual motion)
   "g R" '(lsp-ui-peek-find-references :wk "find references")))

(use-package dap-mode
  :after lsp-mode
  :custom
  (dap-auto-configure-features '(sessions locals controls tooltip))
  :config (dap-auto-configure-mode))

(provide 'lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lsp.el ends here
