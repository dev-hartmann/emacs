(use-package emacs-lisp-mode
  :hook (emacs-lisp-mode . evil-clever-parens-mode)
  :config
  (bind-key "RET" 'comment-indent-new-line emacs-lisp-mode-map)
  (bind-key "C-c c" 'compile emacs-lisp-mode-map))

(add-hook 'emacs- hmode-hook 'lsp-deferred)
(add-hook )
