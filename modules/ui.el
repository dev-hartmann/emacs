;; ui.el --- User interface configuration -*- lexical-binding: t -*-

;; Filename: editor.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


;; Font config
(set-frame-font "Fira Code 12" nil t)

(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme 'doom-dracula) ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package treemacs-nerd-icons
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package hide-mode-line
  :straight (:source melpa))

(use-package diff-hl
  :hook
  (prog-mode . diff-hl-mode))

(use-package highlight-indent-guides
  :commands highlight-indent-guides-mode
  :diminish highlight-indent-guides-mode
  :hook
  (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-even-face-perc 3)
  (highlight-indent-guides-auto-odd-face-perc 2.5)
  (highlight-indent-guides-auto-top-even-face-perc 12)
  (highlight-indent-guides-auto-top-odd-face-perc 10)
  (highlight-indent-guides-character ?\u2502)
  (highlight-indent-guides-method 'column)
  (highlight-indent-guides-responsive 'top))

(setq kind-icon-use-icons nil)
(setq kind-icon-mapping
      `(
        (array ,(nerd-icons-codicon "nf-cod-symbol_array") :face font-lock-type-face)
        (boolean ,(nerd-icons-codicon "nf-cod-symbol_boolean") :face font-lock-builtin-face)
        (class ,(nerd-icons-codicon "nf-cod-symbol_class") :face font-lock-type-face)
        (color ,(nerd-icons-codicon "nf-cod-symbol_color") :face success)
        (command ,(nerd-icons-codicon "nf-cod-terminal") :face default)
        (constant ,(nerd-icons-codicon "nf-cod-symbol_constant") :face font-lock-constant-face)
        (constructor ,(nerd-icons-codicon "nf-cod-triangle_right") :face font-lock-function-name-face)
        (enummember ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
        (enum-member ,(nerd-icons-codicon "nf-cod-symbol_enum_member") :face font-lock-builtin-face)
        (enum ,(nerd-icons-codicon "nf-cod-symbol_enum") :face font-lock-builtin-face)
        (event ,(nerd-icons-codicon "nf-cod-symbol_event") :face font-lock-warning-face)
        (field ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-variable-name-face)
        (file ,(nerd-icons-codicon "nf-cod-symbol_file") :face font-lock-string-face)
        (folder ,(nerd-icons-codicon "nf-cod-folder") :face font-lock-doc-face)
        (interface ,(nerd-icons-codicon "nf-cod-symbol_interface") :face font-lock-type-face)
        (keyword ,(nerd-icons-codicon "nf-cod-symbol_keyword") :face font-lock-keyword-face)
        (macro ,(nerd-icons-codicon "nf-cod-symbol_misc") :face font-lock-keyword-face)
        (magic ,(nerd-icons-codicon "nf-cod-wand") :face font-lock-builtin-face)
        (method ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
        (function ,(nerd-icons-codicon "nf-cod-symbol_method") :face font-lock-function-name-face)
        (module ,(nerd-icons-codicon "nf-cod-file_submodule") :face font-lock-preprocessor-face)
        (numeric ,(nerd-icons-codicon "nf-cod-symbol_numeric") :face font-lock-builtin-face)
        (operator ,(nerd-icons-codicon "nf-cod-symbol_operator") :face font-lock-comment-delimiter-face)
        (param ,(nerd-icons-codicon "nf-cod-symbol_parameter") :face default)
        (property ,(nerd-icons-codicon "nf-cod-symbol_property") :face font-lock-variable-name-face)
        (reference ,(nerd-icons-codicon "nf-cod-references") :face font-lock-variable-name-face)
        (snippet ,(nerd-icons-codicon "nf-cod-symbol_snippet") :face font-lock-string-face)
        (string ,(nerd-icons-codicon "nf-cod-symbol_string") :face font-lock-string-face)
        (struct ,(nerd-icons-codicon "nf-cod-symbol_structure") :face font-lock-variable-name-face)
        (text ,(nerd-icons-codicon "nf-cod-text_size") :face font-lock-doc-face)
        (typeparameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
        (type-parameter ,(nerd-icons-codicon "nf-cod-list_unordered") :face font-lock-type-face)
        (unit ,(nerd-icons-codicon "nf-cod-symbol_ruler") :face font-lock-constant-face)
        (value ,(nerd-icons-codicon "nf-cod-symbol_field") :face font-lock-builtin-face)
        (variable ,(nerd-icons-codicon "nf-cod-symbol_variable") :face font-lock-variable-name-face)
        (t ,(nerd-icons-codicon "nf-cod-code") :face font-lock-warning-face)))

(use-package dashboard
  ;; :hook (dashboard-mode . hide-mode-line-mode)
  :config
  (setq dashboard-startup-banner 3
        dashboard-filter-agenda-entry 'dashboard-no-filter-agenda
        dashboard-items '((projects . 5)
                          (agenda . 5)
                          (recents  . 5))
        dashboard-set-heading-icons t
        dashboard-set-file-icons t)
  (dashboard-setup-startup-hook))


(require 'zoom-frm)
(use-package focus)

(provide 'ui)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ui.el ends here
