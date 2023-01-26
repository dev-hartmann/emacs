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
  (consult-project-root-function #'dh/get-project-root)
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

(use-package zoom-frm)

(transient-define-prefix transient-text-operations ()
  "Text ops"
  ["Zoom"
   ("k" "in" zoom-in :transient t)
   ("j" "out" zoom-out :transient t)])

;; EVIL mode and packages
(use-package general
  :config
  (general-evil-setup)
  (general-create-definer dh/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer dh/local-leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC m"
    :global-prefix "M-SPC")

  (general-define-key
   :states '(normal visual motion)
   "U"  '(undo-tree-visualize :wk "undo tree")
   "g O" '(consult-imenu :wk "imenu")
   "g Z" '(transient-text-operations :wk "zoom frame"))

  (dh/local-leader-keys
    :states 'normal
    :keymaps 'emacs-lisp-mode-map
    "e" '(nil :which-key "(e)val")
    "e b" '(eval-buffer :which-key "eval (b)uffer")
    "e e" '(eval-last-sexp :which-key "eval last s(e)xp")
    "e d" '(eval-defun :which-key "eval (d)efun containing point")
    "e r" '(eval-region :which-key "eval (r)egion"))

  (dh/leader-keys
    "SPC"'(execute-extended-command :wk "execute extended cmd")
    "f"  '(:ignore t :which-key "(f)iles")
    "ff" '(find-file :which-key "find file")
    "s"  '(:ignore t :which-key "(s)earch")
    "s s" '(consult-line :which-key "search in file")
    "s p" '(consult-ripgrep :which-key "search in project (ripgrep)")
    "p"  '(:ignore t :which-key "(p)roject")
    "p f" '(consult-projectile-find-file :which-key "find (f)ile")
    "p d" '(consult-projectile-find-dir :which-key "find (d)ir")
    "p c" '(projectile-compile-project :which-key "(c)ompile project")
    "p t" '(projectile-test-project :which-key "(t)est project")
    "p b" '(consult-projectile-switch-to-buffer :which-key "switch (b)uffer")
    "p p" '(consult-projectile-switch-project :wk "switch (p)roject")
    "b"  '(:ignore t :which-key "(b)uffer")
    "b b" '(consult-project-buffer :which-key "list project buffers")
    "b B" '(consult-buffer :which-key "list all buffers")
    "b k" '(kill-current-buffer :which-key "kill current buffer")
    "b r" '(revert-buffer :which-key "(r)evert current buffer")
    "b R" '(revert-buffer-all :which-key "(r)evert all buffers")
    "b p" '(evil-prev-buffer :which-key "(p)revious buffer")
    "b n" '(evil-next-buffer :which-key "(n)ext buffer")
    "g"   '(:ignore t :which-key "(g)it")
    "g g" '(magit-status :which-key "(g)it status")
    "g b" '(magit-blame :which-key "git (b)lame in file")
    "g d" '(magit-diff :which-key "git (d)iff")
    "t"   '(:ignore t :which-key "(t)oggle")
    "t d" '(toggle-debug-on-error :which-key "debug on error")
    "t l" '(display-line-numbers-mode :wk "line numbers")
    "t t" '(treemacs :which-key "treemacs")
    "t w" '((lambda () (interactive) (toggle-truncate-lines)) :wk "word wrap")
    "t n" '((lambda () (interactive) (setq display-line-numbers-type t)) :wk "standard line numbers")
    "t N" '((lambda () (interactive) (setq display-line-numbers-type 'relative)) :wk "relative line numbers")
    "h"   '(:ignore t :which-key "describe")
    "h e" 'view-echo-area-messages
    "h f" 'describe-function
    "h F" 'describe-face
    "h l" 'view-lossage
    "h L" 'find-library
    "h m" 'describe-mode
    "h k" 'describe-key
    "h K" 'describe-keymap
    "h p" 'describe-package
    "h v" 'describe-variable
    "w"   '(:ignore t wk: "window commands")
    "w v" '(dh/create-vertical-window-and-projectile-find :wk "split (V), move, find")
    "w h" '(windmove-left :wk "move to left window")
    "w l" '(windmove-right :wk "move to right window")
    "w c" '(dh/close-window-and-balance :wk "(c)lose current window")
    "w =" '(balance-windows :wk "balance windows")
    "x"   '(:ignore t :which-key "te(x)t")
    "x z" '(transient-text-operations :which-key "zoom text")
    "o"   '(:ignore t :which-key "Open")
    "q"   '(:ignore t :which-key "Quit")
    "q q"  '(kill-emacs :which-key "quit emacs")
    "q Q"  '(save-buffers-kill-emacs :which-key "save all buffers and quit emacs")))

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  :bind (("<escape>" . keyboard-escape-quit))
  :config
  (evil-mode t)
  (evil-set-undo-system 'undo-fu)
  (setq evil-move-cursor-back nil
        evil-move-beyond-eol t
        evil-want-fine-undo t
        evil-mode-line-format 'before
        evil-normal-state-cursor '(box "orange")
        evil-insert-state-cursor '(box "green")
        evil-visual-state-cursor '(box "#F86155")
        evil-emacs-state-cursor  '(box "purple")))

(use-package evil-multiedit
  :after evil
  :diminish evil-multiedit
  :config
  (evil-multiedit-default-keybinds))

(use-package evil-collection
  :after evil
  :diminish evil-collection
  :config
  (evil-collection-init))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

(use-package evil-goggles
  :after evil
  :demand
  :init
  (setq evil-goggles-duration 0.2)
  :config
  (push '(evil-operator-eval
          :face evil-goggles-yank-face
          :switch evil-goggles-enable-yank
          :advice evil-goggles--generic-async-advice)
        evil-goggles--commands)
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package evil-commentary
  :after evil
  :diminish
  :config (evil-commentary-mode +1))


(use-package winum
  :config (winum-mode 1))

(use-package smartparens
  :init (require 'smartparens-config)
  :diminish smartparens-mode
  :hook (prog-mode . smartparens-mode))

;; We don't actually enable cleverparens, because most of their bindings we
;; don't want, we install our own bindings for specific sexp movements
(use-package evil-cleverparens
  :after (evil smartparens))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :hook ((clojurex-mode
          clojurescript-mode
          clojurec-mode
          clojure-mode
          emacs-lisp-mode
          lisp-data-mode)
         . aggressive-indent-mode))

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

(use-package lsp-mode
  :commands
  (lsp lsp-deferred)
  :hook
  ((lsp-mode . (lambda () (setq-local evil-lookup-func #'lsp-describe-thing-at-point)))
   (lsp-mode . lsp-enable-which-key-integration))
  :general
  (dh/leader-keys
    :states 'normal
    :keymaps 'lsp-mode-map
    "c" '(:ignore t :which-key "code")
    "c l" '(:keymap lsp-command-map :wk "lsp")
    "c a" '(lsp-execute-code-action :wk "code action")
    "c r" '(lsp-rename :wk "rename"))
  :init
  (setq lsp-restart 'ignore)
  (setq lsp-eldoc-enable-hover t)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-auto-execute-action nil)
  (setq lsp-before-save-edits t)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-file-watch-ignored-directories (append lsp-file-watch-ignored-directories (list ".*/\.cache/.*"))))

(use-package lsp-ui
  :hook
  ((lsp-mode . lsp-ui-mode))
  :general (dh/local-leader-keys
             "h" 'lsp-ui-doc-show
             "H" 'lsp-ui-doc-hide)
  (lsp-ui-peek-mode-map
   :states 'normal
   "C-j" 'lsp-ui-peek--select-next
   "C-k" 'lsp-ui-peek--select-prev)
  (outline-mode-map
   :states 'normal
   "C-j" 'nil
   "C-k" 'nil)
  :init
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-show-with-mouse t)
  (setq lsp-ui-peek-always-show t)
  (setq lsp-ui-peek-fontify 'always)
  (setq lsp-file-watch-ignored-directories (append lsp-file-watch-ignored-directories (list ".*/\.cache/.*"))))

(use-package dap-mode
  :after lsp-mode
  :custom
  (dap-auto-configure-features '(sessions locals controls tooltip))
  :config (dap-auto-configure-mode))

                                        ;VC and Git
(use-package magit
  :bind ("C-x g" . magit-status)
  :config (add-hook 'with-editor-mode-hook #'evil-insert-state))

(use-package forge
  :after magit)

(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode))

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
(use-package company
  :diminish company-mode
  :hook  ((prog-mode . company-mode)
          (lsp-mode . company-mode))
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1)
  :custom
  (company-show-numbers t)
  (company-tooltip-idle-delay 0.25 "Faster!")
  (company-async-timeout 20 "Some requests take longer"))

(use-package company-box
  :hook (company-mode . company-box-mode))

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

(use-package kubernetes-helm
  :after kubernetes)

(use-package revert-buffer-all)

(use-package sql)

(use-package sqlformat
  :commands (sqlformat sqlformat-buffer sqlformat-region)
  :hook (sql-mode . sqlformat-on-save-mode)
  :init
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("-s2" "-g" "-u1")))
