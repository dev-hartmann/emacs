;; `bootstrap.el' contains boilerplate code related to package management. You
;; can follow the same pattern if you want to split out other bits of config.
(load-file (expand-file-name "bootstrap.el" user-emacs-directory))
(load-file (expand-file-name "defaults.el" user-emacs-directory))
(load-file (expand-file-name "custom-functions.el" user-emacs-directory))
(load-file (expand-file-name "editor.el" user-emacs-directory))
(load-file (expand-file-name "ui.el" user-emacs-directory))
(load-file (expand-file-name "lang-clojure.el" user-emacs-directory))
;; (load-file (expand-file-name "org.el" user-emacs-directory))

(use-package exec-path-from-shell
  :custom
  (exec-path-from-shell-variables '("PATH"
                                    "MANPATH"
                                    "TMPDIR"
                                    "KUBECONFIG"
                                    "GOPATH"
                                    "GOBIN"
                                    "GOROOT"
                                    "GOPRIVATE"
                                    "GOENV_GOPATH_PREFIX"
                                    "GOENV_VERSION"))
  (exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-debug nil)
  :config
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

(setq use-package-compute-statistics t)

;; Handle emacs garbage collection for me
(use-package gcmh
  :demand
  :config
  (gcmh-mode 1))

;; keep .emacs.d/ clean
(use-package no-littering
  :demand
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))

;; Language-specific packages
(use-package org
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)))

(use-package org-bullets :hook (org-mode . org-bullets-mode))

(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode))

(use-package yaml-mode)

(server-start)

;; Enable transparent titlebar
(use-package ns-auto-titlebar
  :config
  (ns-auto-titlebar-mode))

;; Enable use of macOS trash
(use-package osx-trash
  :custom
  (delete-by-moving-to-trash t)
  :config
  (osx-trash-setup))

;; Not a fan of trailing whitespace in source files, strip it out when saving.
(use-package whitespace
  :ensure nil
  :hook (before-save . whitespace-cleanup))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(custom-safe-themes
   '("c865644bfc16c7a43e847828139b74d1117a6077a845d16e71da38c8413a5aaa" default))
 '(safe-local-variable-values
   '((elisp-lint-indent-specs
      (if-let* . 2)
      (when-let* . 1)
      (let* . defun)
      (nrepl-dbind-response . 2)
      (cider-save-marker . 1)
      (cider-propertize-region . 1)
      (cider-map-repls . 1)
      (cider--jack-in . 1)
      (cider--make-result-overlay . 1)
      (insert-label . defun)
      (insert-align-label . defun)
      (insert-rect . defun)
      (cl-defun . 2)
      (with-parsed-tramp-file-name . 2)
      (thread-first . 0)
      (thread-last . 0))
     (eval add-hook 'before-save-hook #'cider-format-buffer)
     (cider-clojure-cli-aliases . ":repl")
     (cider-format-code-options
      ("indents"
       (("grpc-service"
         (("inner" 0))))))
     (cider-clojure-cli-global-options . "-M:repl")
     (cider-preferred-build-tool . clojure-cli)
     (checkdoc-package-keywords-flag)))
 '(warning-suppress-log-types '((lsp-mode)))
 '(warning-suppress-types '((use-package) (use-package) (use-package) (lsp-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
