(use-package rustic
  :mode ("\\.r$" . rustic-mode)
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  :config
  (setq rustic-format-on-save t))

(provide 'lang-rust)
