(use-package elixir-mode
 :hook (elixir-mode . lsp))

(use-package alchemist
 :hook (elixir-mode . alchemist)
 :config 
  (set-lookup-handlers! 'elixir-mode
    :definition #'alchemist-goto-definition-at-point
    :documentation #'alchemist-help-search-at-point)
  (set-eval-handler! 'elixir-mode #'alchemist-eval-region)
  (set-repl-handler! 'elixir-mode #'alchemist-iex-project-run)
  (setq alchemist-mix-env "dev")
  (setq alchemist-hooks-compile-on-save t))

(use-package exunit)

(use-package flycheck-credo
 :after flycheck 
 :config 
(flycheck-credo-setup))

(provide 'elixir-lang)
