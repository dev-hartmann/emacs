;; vim.el --- Evil vim bindings and general config -*- lexical-binding: t -*-

;; Filename: vim.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'thingatpt)
(defun dh/search-thing-at-point-in-project ()
  (interactive)
  (let ((project-root (projectile-project-root)))
    (when project-root
      (consult-ripgrep project-root (thing-at-point 'symbol)))))

(defun dh/search-thing-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(defun dh/create-vertical-window-and-projectile-find ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (consult-projectile-find-file)
  (balance-windows))

(defun dh/close-window-and-balance ()
  (interactive)
  (delete-window)
  (balance-windows))

(use-package transient)

(use-package zoom-frm)

(transient-define-prefix transient-text-operations ()
  "Text ops"
  ["Zoom"
   ("k" "in" zoom-in :transient t)
   ("j" "out" zoom-out :transient t)])

(use-package general
  :config
  (general-evil-setup)
  (general-create-definer dh/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer dh/local-leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix ","
    :global-prefix "M-SPC")

  (general-define-key
   :states '(normal visual motion)
   "U"  '(undo-tree-visualize :wk "undo tree")
   "g O" '(consult-imenu :wk "imenu")
   "g Z" '(transient-text-operations :wk "zoom frame")
   "g r" '(lsp-ui-peek-find-references :w "find references"))

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
    "u"  '(universal-argument :wk "universal argument")
    "f"  '(:ignore t :which-key "(f)iles")
    "ff" '(find-file :which-key "find file")
    "s"  '(:ignore t :which-key "(s)earch")
    "s s" '(dh/search-thing-at-point :which-key "search in file")
    "s p" '(dh/search-thing-at-point-in-project :which-key "search in project (ripgrep)")
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
    "x p" '(:ignore t :wk "profiler")
    "x p s" '(profiler-start :which-key "profiler start")
    "x p S" '(transient-text-operations :which-key "profiler stop")
    "o" '(nil :which-key "(o)rg mode")
    "o T" '(dh/open-org-tasks-file :which-key "(o)rg mode")
    "o L" '(org-todo-list  :which-key "org (L)ist todos")
    "o t" '(org-todo :which-key "org (t)oggle todo state")
    "o n" '(org-add-note  :which-key "org (a)dd note to todo")
    "o c" '(org-toggle-checkbox  :which-key "org toggle (c)heckbox")
    "q"   '(:ignore t :which-key "Quit")
    "q q"  '(kill-emacs :which-key "quit emacs")
    "q Q"  '(save-buffers-kill-emacs :which-key "save all buffers and quit emacs")))

;; unset C-u since we have bound it to SPC-u
(global-unset-key (kbd "C-u"))
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


(provide 'vim)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vim.el ends here
