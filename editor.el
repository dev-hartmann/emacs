;; editor.el --- User interface configuration for Corgi -*- lexical-binding: t -*-

;; Filename: editor.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(require 'use-package)
(require 'custom-functions)

(use-package eldoc
  :hook (emacs-lisp-mode cider-mode))

(use-package diminish
  :diminish
  elisp-slime-nav-mode
  eldoc-mode
  subword-mode)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1)
  (setq which-key-sort-order 'which-key-prefix-then-key-order))

(use-package helpful
  :after evil
  :init
  (setq evil-lookup-func #'helpful-at-point)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package imenu
  :straight (:type built-in)
  :defer t
  :custom
  (imenu-auto-rescan t)
  (imenu-max-item-length 160)
  (imenu-max-items 400))

(use-package consult
  :demand t
  :custom
  (completion-in-region-function #'consult-completion-in-region))

(use-package vertico
  :custom
  (Vertico-cycle t)
  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package embark
  :bind (("C-S-a" . embark-act)
         :map minibuffer-local-map
         ("C-d" . embark-act))
  :config
  ;; Show Embark actions via which-key
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(require 'thingatpt)
(defun dh/search-thing-at-point-in-project ()
  (interactive)
  (let ((project-root (projectile-project-root)))
    (when project-root
      (consult-ripgrep project-root (thing-at-point 'symbol)))))

(defun dh/search-thing-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(use-package treemacs
  :config
  (setq treemacs-follow-after-init t)
  (treemacs-project-follow-mode)
  (treemacs-git-mode 'simple))

(use-package treemacs-evil)

(use-package treemacs-projectile)

(use-package avy)

(use-package undo-fu)

(use-package undo-tree
  :straight t
  :diminish undo-tree
  :custom
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  :config
  (global-undo-tree-mode))

(advice-add 'undo-tree-visualize :around #'dh/undo-tree-split-side-by-side)

(use-package transient)

(use-package zoom-frm)

(transient-define-prefix transient-text-operations ()
  "Text ops"
  ["Zoom"
   ("k" "in" zoom-in :transient t)
   ("j" "out" zoom-out :transient t)])

(use-package winum
  :config (winum-mode 1))

(use-package smartparens
  :init (require 'smartparens-config)
  :diminish smartparens-mode
  :hook (prog-mode . smartparens-mode))

(use-package evil-smartparens
  :after smartparens)

;; We don't actually enable cleverparens, because most of their bindings we
;; don't want, we install our own bindings for specific sexp movements
(use-package evil-cleverparens
  :after (evil smartparens))

(use-package rainbow-delimiters
  :hook ((cider-repl-mode
          clojurex-mode
          clojurescript-mode
          clojurec-mode
          clojure-mode
          emacs-lisp-mode
          lisp-data-mode
          inferior-emacs-lisp-mode)
         . rainbow-delimiters-mode))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :hook ((clojurex-mode
          clojurescript-mode
          clojurec-mode
          clojure-mode
          emacs-lisp-mode
          lisp-data-mode)
         . aggressive-indent-mode))

(use-package tree-sitter
  :if (executable-find "tree-sitter")
  :hook (((rustic-mode
           python-mode
           go-mode
           typescript-mode
           css-mode) . tree-sitter-mode)
         ((rustic-mode
           python-mode
           go-mode
           typescript-mode
           css-mode) . tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :if (executable-find "tree-sitter")
  :after tree-sitter)

(use-package super-save
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(use-package magit
  :bind ("C-x g" . magit-status)
  :config (add-hook 'with-editor-mode-hook #'evil-insert-state))

(use-package forge
  :after magit)

(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode))

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

;; VC and Git
;; Snippets
;; (use-package yasnippet
;;   :straight t
;;   :config
;;   (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
;;   (yas-reload-all)
;;   (add-hook 'prog-mode-hook #'yas-minor-mode))

;; (use-package yasnippet-snippets
;;   :after yasnippet
;;   :straight t
;;   :config
;;   (yasnippet-snippets-initialize))

;; Completion frontend
;; Popup completion-at-point
(use-package corfu
  :straight (corfu :files (:defaults "extensions/*")
                   :includes (corfu-info corfu-history))
  :general
  (:keymaps 'corfu-map
            :states '(insert normal visual)
            "<tab>" #'corfu-next
            "<escape>" #'corfu-quit
            "<return>" #'corfu-insert)
  :custom
  (corfu-auto-delay 0.25)
  (corfu-cycle t)
  (corfu-auto t)
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Project management
(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode)
  (setq projectile-create-missing-test-files t))

(use-package consult-projectile)

;; Checker
(use-package flycheck
  :init
  (setq flycheck-indication-mode 'right-fringe)
  (global-flycheck-mode))

(use-package dumb-jump)

(use-package goto-last-change)

(use-package expand-region)

(use-package bazel)

(use-package kubernetes)

(use-package revert-buffer-all)

(use-package sql)

(use-package sqlformat
  :commands (sqlformat sqlformat-buffer sqlformat-region)
  :hook (sql-mode . sqlformat-on-save-mode)
  :init
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("-s2" "-g" "-u1")))

(use-package shell-pop)

(use-package nix-mode)
