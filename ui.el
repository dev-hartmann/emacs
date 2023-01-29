(use-package all-the-icons)

  (use-package doom-themes
    :ensure t
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled
    (load-theme 'doom-vibrant)
    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    ;; Enable custom neotree theme (all-the-icons must be installed!)
    (doom-themes-neotree-config)
    ;; or for treemacs users
    (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
    (doom-themes-treemacs-config)
    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package hide-mode-line
  :straight (:source melpa))

(use-package diff-hl
  :straight t
  :hook
  (prog-mode . diff-hl-mode))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :if window-system
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode)

  :config
  ;; Override default category lookup function.
  (defun all-the-icons-completion-get-icon (cand cat)
    "Return the icon for the candidate CAND of completion category CAT."
    (cl-case cat
      (file (all-the-icons-completion-get-file-icon cand))
      (project-file (all-the-icons-completion-get-file-icon cand))
      (buffer (all-the-icons-completion-get-buffer-icon cand))
      (project-buffer (all-the-icons-completion-get-buffer-icon cand))
      (t ""))))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(defun halve-other-window-height ()
  "Expand current window to use half of the other window's lines."
  (interactive)
  (enlarge-window (/ (window-height (next-window)) 3)))

(defun dh/console-window-setup ()
  (interactive)
  (split-window-below)
  (halve-other-window-height)
  (windmove-down)
  (shell)
  (windmove-up)
  (magit-status))

(use-package highlight-indent-guides
  :defer t
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



(setq display-line-numbers-type 'visual)

(require 'zoom-frm)

(defvar screen-share nil)

(defun add-val-to-global-mode-string (str)
  (setq global-mode-string
        (cond ((consp global-mode-string)
               (add-to-list 'global-mode-string str 'APPEND))
              ((not global-mode-string)
               (list str))
              ((stringp global-mode-string)
               (list global-mode-string str)))))

(defun add-to-global-mode-string (val)
  (if (listp val)
      (dolist (e val)
        (add-val-to-global-mode-string e))
    (add-val-to-global-mode-string val)))

(defun remove-from-global-mode-string (str)
  (add-to-global-mode-string (remove-from-list-by-substring str global-mode-string)))

(defun reset-global-mode-string ()
  (setq global-mode-string ""))

(defun remove-from-list-by-substring (substring list)
  (let ((matching-items (cl-remove-if-not (lambda (item) (string-match-p substring item)) list)))
    (dolist (item matching-items)
      (setq list (delete item list)))
    list))

(defun update-timer (start-time end-time)
  (let ((time-left (time-subtract end-time (current-time))))
    (if (time-less-p time-left '(0 0 0))
        (progn
          (cancel-function-timers 'update-timer)
          (add-to-global-mode-string (remove-from-list-by-substring "Time left:" global-mode-string))
          (add-to-global-mode-string "Time's up!"))
      (progn
        (add-to-global-mode-string (remove-from-list-by-substring "Time left:" global-mode-string))
        (add-to-global-mode-string (format "Time left: %d:%02d" (nth 1 time-left) (nth 0 time-left)))))))

(defun countdown-timer (minutes)
  (run-at-time "1 sec" 1 'update-timer (round (float-time)) (+ (round (float-time)) (* 60 minutes)))
  (message "Timer started for %d minutes" minutes))

(defun dh/toggle-screen-share (&optional minutes)
  (interactive "p")
  (setq frame-zoom-font-difference 3)
  (let ((str  (list " 🔥 " ))
        (mins (or minutes 30)))
    (if screen-share
        (progn
          (setq screen-share nil)
          (zoom-frm-out)
          (setq display-line-numbers-type 'relative)
          (reset-global-mode-string)
          (cancel-function-timers 'update-timer))
      (progn
        (setq screen-share t)
        (zoom-frm-in)
        (setq display-line-numbers-type t)
        (add-to-global-mode-string str)
        (countdown-timer mins))
      (revert-buffer-all)
      (message (concat "screen sharing: " (if screen-share "activated" "disabled"))))))


(defun mob-status ()
  "Show the current status of the mob session in a buffer named `*mob-status*`."
  (interactive)
  (let ((buffer (get-buffer-create "*mob-status*")))
    (with-current-buffer buffer
      (erase-buffer)
      (call-process "mob" nil t nil "status")
      (goto-char (point-min)))
    (pop-to-buffer buffer)))


(provide 'ui)
