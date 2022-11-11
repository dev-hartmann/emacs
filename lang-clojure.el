;;; corgi-clojure.el --- Clojure configuration for Corgi -*- lexical-binding: t -*-
;;
;; Filename: corgi-clojure.el
;; Package-Requires: ((use-package) (cider) (clj-ns-name) (clojure-mode))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(use-package flycheck-clj-kondo)

(use-package clojure-mode
  :magic ("^#![^\n]*/\\(clj\\|clojure\\|bb\\|lumo\\)" . clojure-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.ednl$" . clojure-mode))
  :config
  (require 'flycheck-clj-kondo)
  (setq clojure-toplevel-inside-comment-form t
        ;; Because of CIDER's insistence to send forms to all linked REPLs, we
        ;; *have* to be able to switch cljc buffer to clj/cljs mode without
        ;; cider complaining.
        clojure-verify-major-mode nil))

(use-package cider
  :custom
  (cider-show-error-buffer nil)
  (cider-eval-result-duration 'change)
  :config
  (evil-collection-cider-setup)
  :general
  (dh/local-leader-keys
    :states 'normal
    :keymaps 'clojure-mode-map
    "'" '(cider-jack-in :wk "cider jack in clj")
    "\"" '(cider-jack-in-cljs :wk "cider jack in cljs")
    "e" '(nil :which-key "(e)val")
    "e b" '(cider-eval-buffer :which-key "eval (b)uffer")
    "e e" '(cider-eval-last-sexp :which-key "eval last s(e)xp")
    "e r" '(cider-eval-list-at-point :which-key "eval oute(r)most sexp")
    "t"   '(:ignore t :wk "(t)est")
    "t t" '(cider-test-run-loaded-tests :wk "run (t)est at point")
    "t f" '(cider-test-rerun-failed-tests :wk "re-run (f)ailed tests")
    "t r" '(cider-test-rerun-tests :wk "(r)e-run tests")
    "t n" '(cider-test-run-ns-tests :wk "run (n)amespace tests")
    "t p" '(cider-test-run-project-tests :wk "run all (p)roject tests")))

(defadvice cider-find-var (before add-evil-jump activate)
  (evil-set-jump))

(use-package clj-ns-name
  :config
  (clj-ns-name-install))

(use-package clojure-snippets)

(provide 'lang-clojure)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; corgi-clojure.el ends here
