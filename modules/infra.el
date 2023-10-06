;;; infra.el --- Infrastructure packages configuration  -*- lexical-binding: t -*-
;;
;; Filename: infra.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
(use-package docker)

(use-package bazel)

(use-package kubernetes
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(use-package kubernetes-evil
  :after kubernetes)

(provide 'infra)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; infra.el ends here
