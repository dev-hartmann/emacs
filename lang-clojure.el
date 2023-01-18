;;; lang-clojure.el --- Clojure configuration  -*- lexical-binding: t -*-
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

(add-hook 'clojure-mode-hook 'eglot)
                                        ;(add-hook 'clojurescript-mode-hook 'lsp-deferred)
                                        ;(add-hook 'clojurec-mode-hook 'lsp-deferred)
(add-hook 'clojure-mode-hook #'evil-cleverparens-mode)

(transient-define-prefix clj-cycle-coll-transient ()
  "Text ops"
  ["Cycle coll type"
   ("[" "vec" clojure-convert-collection-to-vector :transient t)
   ("(" "list" clojure-convert-collection-to-list :transient t)
   ("{" "map" clojure-convert-collection-to-map :transient t)
   ("#" "set" clojure-convert-collection-to-set :transient t)
   ("'" "quoted list" clojure-convert-collection-to-quoted-list :transient t)])

(use-package cider
  :diminish cider-mode
  :custom
  (cider-show-error-buffer nil)
  (cider-eval-result-duration 'change)
  :config
  (setq cider-preferred-build-tool 'clojure-cli
        cider-font-lock-dynamically nil
        cider-eldoc-display-for-symbol-at-point nil
        cider-use-xref nil)
  (evil-collection-cider-setup)
  :general
  (dh/local-leader-keys
    :states '(normal visual)
    :keymaps 'clojure-mode-map
    "'" '(cider-jack-in :wk "cider jack in clj")
    "\"" '(cider-jack-in-cljs :wk "cider jack in cljs")
    "e" '(nil :which-key "(e)val")
    "e b" '(cider-eval-buffer :which-key "eval (b)uffer")
    "e e" '(cider-eval-last-sexp :which-key "eval last s(e)xp")
    "e r" '(cider-eval-list-at-point :which-key "eval oute(r)most sexp")
    "t"   '(:ignore t :wk "(t)est")
    "t t" '(cider-test-run-test :wk "run (t)est at point")
    "t f" '(cider-test-rerun-failed-tests :wk "re-run (f)ailed tests")
    "t r" '(cider-test-rerun-tests :wk "(r)e-run tests")
    "t n" '(cider-test-run-ns-tests :wk "run (n)amespace tests")
    "t p" '(cider-test-run-project-tests :wk "run all (p)roject tests")
    "c"   '(:ignore t :wk "(c)ode actions")
    "c m" '(cljr-add-missing-libspec :wk "try add (m)issing require for thing at point")
    "c c" '(clj-cycle-coll-transient :wk "cycle (c)ollection type for thing at point")
    "c e" '(:ignore t :wk "(e)xtract")
    "c e d" '(cljr-extract-def :wk "extract (d)ef")
    "c e f" '(cljr-extract-function :wk "extract (f ")
    "r"   '(:ignore t :wk "(r)epl")
    "r q" '(cider-quit :wk "(q)uit cider")
    "r Q" '(corgi/cider-quit-all :wk "(Q)uit all cider repls")
    "r r" '(cider-restart :wk "(r)estart cider")
    "r n" '(cider-repl-set-ns :wk "set repl to current (n)amespace")
    "r i" '(cider-interrupt :wk "(i)nterupt repl evaluation")))

(use-package clj-refactor
  :after cider
  :diminish clj-refactor-mode
  :config
  (setq cljr-warn-on-eval nil
        cljr-eagerly-build-asts-on-startup nil)
  :general
  (general-define-key
   :states '(normal visual motion)
   "g R" '(cljr-find-usages :wk "find (R)eferences")))

(defadvice cider-find-var (before add-evil-jump activate)
  (evil-set-jump))

(use-package kaocha-runner
  :general
  (dh/local-leader-keys
    :states  '(normal visual)
    :keymaps 'clojure-mode-map
    "k" '(:ignore t :wk "kaocha tests")
    "k t" '(kaocha-runner-run-test-at-point :wk "run (t)est at point")
    "k s" '(cider-test-rerun-tests :wk "(s)how warnings from test/s")
    "k n" '(kaocha-runner-run-tests :wk "run (n)amespace tests")
    "k p" '(kaocha-runner-run-all-tests :wk "run all (p)roject tests")))

(use-package clj-ns-name
  :config
  (clj-ns-name-install))

(use-package clojure-snippets)

;; Leverage an existing cider nrepl connection to evaluate portal.api functions
;; and map them to convenient key bindings.

;; def portal to the dev namespace to allow dereferencing via @dev/portal
(defun portal.api/open ()
  (interactive)
  (cider-nrepl-sync-request:eval
   "(do (ns dev) (def portal ((requiring-resolve 'portal.api/open))) (add-tap (requiring-resolve 'portal.api/submit)))"))

(defun portal.api/clear ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/clear)"))

(defun portal.api/close ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/close)"))

;; Backwards compatibility
(defalias 'corgi/cider-pprint-register #'corgi/cider-pprint-eval-register)

(defun corgi/cider-jack-in-babashka (&optional project-dir)
  "Start a utility CIDER REPL backed by Babashka, not related to a
specific project."
  (interactive)
  (let ((project-dir (or project-dir user-emacs-directory)))
    (nrepl-start-server-process
     project-dir
     "bb --nrepl-server 0"
     (lambda (server-buf)
       (set-process-query-on-exit-flag
        (get-buffer-process server-buf) nil)
       (cider-nrepl-connect
        (list :repl-buffer server-buf
              :repl-type 'clj
              :host (plist-get nrepl-endpoint :host)
              :port (plist-get nrepl-endpoint :port)
              :project-dir project-dir
              :session-name "babashka"
              :repl-init-function (lambda ()
                                    (setq-local cljr-suppress-no-project-warning t
                                                cljr-suppress-middleware-warnings t
                                                process-query-on-exit-flag nil)
                                    (set-process-query-on-exit-flag
                                     (get-buffer-process (current-buffer)) nil)
                                    (rename-buffer "*babashka-repl*"))))))))

(defun corgi/cider-quit-all ()
  "Quit all current CIDER REPLs."
  (interactive)
  (let ((repls (seq-remove (lambda (r)
                             (equal r (get-buffer "*babashka-repl*")))
                           (seq-mapcat #'cdr (sesman-current-sessions 'CIDER)))))
    (seq-do #'cider--close-connection repls))
  ;; if there are no more sessions we can kill all ancillary buffers
  (cider-close-ancillary-buffers)
  ;; need this to refresh sesman browser
  (run-hooks 'sesman-post-command-hook))


(provide 'lang-clojure)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lang-clojure.el ends here
