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
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :general (general-define-key
            :states 'normal
            "g l"   '(:ignore t :which-key "code")
            "g l l" '(:keymap lsp-command-map :wk "lsp")
            "g l a" '(lsp-execute-code-action :wk "code action")
            "g l r" '(lsp-rename :wk "rename"))
  :init
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-before-save-edits t)
  (setq lsp-headerline-breadcrumb-enable nil))

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
(use-package yasnippet
  :straight t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets
  :after yasnippet
  :straight t
  :config
  (yasnippet-snippets-initialize))

;; Completion frontend
(use-package hippie-exp
  :bind ([remap dabbrev-expand] . hippie-expand)
  :commands (hippie-expand)
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"))
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line)))

(use-package corfu
  :straight
  (corfu :files (:defaults "extensions/*")
         :includes (corfu-info corfu-echo corfu-history corfu-popupinfo))
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert) ; Do not preview current candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . nil))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  (corfu-echo-mode))

(use-package cape
  :defer 10
  :bind ("C-c f" . cape-file)
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (defalias 'dabbrev-after-2 (cape-capf-prefix-length #'cape-dabbrev 2))
  (add-to-list 'completion-at-point-functions 'dabbrev-after-2 t)
  (cl-pushnew #'cape-file completion-at-point-functions)
  :config
  ;; Silence then pcomplete capf, no errors or messages!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Ensure that pcomplete does not write to the buffer
  ;; and behaves as a pure `completion-at-point-function'.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package cape-yasnippet
  :straight (cape-yasnippet :type git :host github :repo "elken/cape-yasnippet")
  :after yasnippet
  :hook ((prog-mode . yas-setup-capf)
         (text-mode . yas-setup-capf)
         (lsp-mode  . yas-setup-capf)
         (sly-mode  . yas-setup-capf))
  :bind (("C-c y" . cape-yasnippet)
         ("M-+"   . yas-insert-snippet))
  :config
  (defun yas-setup-capf ()
    (setq-local completion-at-point-functions
                (cons 'cape-yasnippet
                      completion-at-point-functions)))
  (push 'cape-yasnippet completion-at-point-functions))

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
