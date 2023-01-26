;;; defaults.el --- Sensible defaults for Emacs -*- lexical-binding: t -*-
;;
;; Filename: defaults.el
;; Description: Sensible defaults for Emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;; Code:

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-startup-message t)

;; Allow pasting selection outside of Emacs
(setq select-enable-clipboard t)

;; Auto refresh buffers
;; Also auto refresh dired, but be quiet about it
(require 'autorevert)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
(add-hook 'dired-mode-hook #'auto-revert-mode)
(global-auto-revert-mode 1)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; write over selected text on input... like all modern editors do
(delete-selection-mode t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; enable recent files mode.
(recentf-mode t)
(setq recentf-exclude `(,(expand-file-name "straight/build/" user-emacs-directory)
                        ,(expand-file-name "eln-cache/" user-emacs-directory)
                        ,(expand-file-name "etc/" user-emacs-directory)
                        ,(expand-file-name "var/" user-emacs-directory)))

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8)   ; pretty
(set-terminal-coding-system 'utf-8)  ; pretty
(set-keyboard-coding-system 'utf-8)  ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8)        ; with sugar on top

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Always display line and column numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)
(set-default 'fill-column 80)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 2000000)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; No electric indent
(setq electric-indent-mode nil)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; Require last line to be newline
(setq require-final-newline t)

;; Version Control
;; Stop asking about following symlinks to version controlled files
(setq vc-follow-symlinks t)
(setq vc-make-backup-files nil)
(setq version-control t)

;; stop emacs from littering the file system with backup files
(setq make-backup-files nil
      auto-save-default t
      create-lockfiles nil)

(when (not (file-directory-p (expand-file-name "backups" user-emacs-directory)))
  (make-directory (expand-file-name "backups" user-emacs-directory)))

(when (not (file-directory-p (expand-file-name "auto-save-list" user-emacs-directory)))
  (make-directory (expand-file-name "auto-save-list" user-emacs-directory)))

;; Put backups and auto-save files in subdirectories, so the
;; user-emacs-directory doesn't clutter
(setq backup-directory-alist
      `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t)))

;; Configure common Emoji fonts, making it more likely that Emoji will work out of the box
(set-fontset-font t 'symbol "Apple Color Emoji")
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)

;; Allow custom themes without asking for permission on startup
(setq custom-safe-themes t)

;; 1mb
(setq read-process-output-max (* 1024 1024))

;; enable winner mode globally for undo/redo window layout changes
(winner-mode t)
(save-place-mode t)
(show-paren-mode t)

;; less noise when compiling elisp
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
(setq native-comp-async-report-warnings-errors nil)
(setq load-prefer-newer t)

(setq ring-bell-function 'ignore)

;; Configure mac modifiers to be what you expect, and turn off the bell noise
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'super)
  (setq mac-option-key-is-meta t)
  (setq mac-option-modifier 'meta)
  (setq mac-right-option-modifier nil)
  (global-set-key (kbd "s-s") 'save-buffer)             ;; save
  (global-set-key (kbd "s-S") 'write-file)              ;; save as
  (global-set-key (kbd "s-q") 'save-buffers-kill-emacs) ;; quit
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-v") 'yank) ;; quit
  (global-set-key (kbd "s-c") 'kill-ring-save);; quit
  (global-set-key (kbd "s-x") 'kill-region))

(provide 'defaults)
