;; Prepare load path
(defvar core-dir (expand-file-name "core" user-emacs-directory) "modules folder containing custom modules")

(defvar modules-dir (expand-file-name "modules" user-emacs-directory) "modules folder containing custom modules")

(add-to-list 'load-path core-dir)

(add-to-list 'load-path modules-dir)

(defun load-directory (dir)
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

;; load core dependencies
(load-directory core-dir)

;; set custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;; load modules
(load-directory modules-dir)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

;; Handle emacs garbage collection for me
(use-package gcmh
  :demand
  :config
  (gcmh-mode 1))

;; keep .emacs.d/ clean
(use-package no-littering
  :config
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory)))


(use-package markdown-mode
  :hook (markdown-mode . visual-line-mode))

(use-package yaml-mode)

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
