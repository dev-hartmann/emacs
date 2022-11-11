;; `bootstrap.el' contains boilerplate code related to package management. You
;; can follow the same pattern if you want to split out other bits of config.
(load-file (expand-file-name "bootstrap.el" user-emacs-directory))
(load-file (expand-file-name "defaults.el" user-emacs-directory))
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

;; Enabling desktop-save-mode will save and restore all buffers between sessions
(setq desktop-restore-frames nil)
(desktop-save-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c865644bfc16c7a43e847828139b74d1117a6077a845d16e71da38c8413a5aaa" default))
 '(safe-local-variable-values '((checkdoc-package-keywords-flag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
