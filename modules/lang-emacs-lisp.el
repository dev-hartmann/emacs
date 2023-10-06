(use-package lispy)

(use-package lispyville
  :after lispy)

(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'emacs-lisp-mode 'lsp-deferred)
