;; `bootstrap.el' contains boilerplate code related to package management. You
;; can follow the same pattern if you want to split out other bits of config.
(load-file (expand-file-name "bootstrap.el" user-emacs-directory))
(load-file (expand-file-name "defaults.el" user-emacs-directory))
(load-file (expand-file-name "custom-functions.el" user-emacs-directory))
(load-file (expand-file-name "vim.el" user-emacs-directory))
(load-file (expand-file-name "editor.el" user-emacs-directory))
(load-file (expand-file-name "ui.el" user-emacs-directory))
(load-file (expand-file-name "lang-clojure.el" user-emacs-directory))
(load-file (expand-file-name "lang-go.el" user-emacs-directory))
(load-file (expand-file-name "lang-rust.el" user-emacs-directory))
(load-file (expand-file-name "elixir-lang.el" user-emacs-directory))
;; (load-file (expand-file-name "org.el" user-emacs-directory))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(custom-safe-themes
   '("1781e8bccbd8869472c09b744899ff4174d23e4f7517b8a6c721100288311fa5" "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" "00cec71d41047ebabeb310a325c365d5bc4b7fab0a681a2a108d32fb161b4006" "7e068da4ba88162324d9773ec066d93c447c76e9f4ae711ddd0c5d3863489c52" "545ab1a535c913c9214fe5b883046f02982c508815612234140240c129682a68" "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644" "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525" "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "c865644bfc16c7a43e847828139b74d1117a6077a845d16e71da38c8413a5aaa" default))
 '(ignored-local-variable-values '((buffer-save-without-query . t)))
 '(safe-local-variable-values
   '((eglot-workspace-configuration
      (:gopls :directoryFilters
              ["-bazel-bin" "-bazel-out" "-bazel-testlogs" "-bazel-core"]
              :gofumpt t :usePlaceholders t :allowModfileModifications t))
     (elisp-lint-indent-specs
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
