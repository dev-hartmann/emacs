(defun mob-available-p ()
  "Tests if the `mob` command is available."
  (eq 0 (call-process "which" nil nil nil "mob")))

(defun mob (&optional minutes)
  "Start a new mob session.
  MINUTES is the optional parameter for the mob start timer"
  (interactive "P")
  (if (not (mob-available-p))
      (message "mob.sh is not available, please install it first.")
    (let ((default-directory (mob-default-directory)))
      (async-shell-command (concat "mob " (if minutes (concat "-t " (number-to-string minutes) " "))  (read-string "Enter mob session name: "))))))

(defun mob-start (&optional minutes)
  "Start a new mob session with optional timer of MINUTES."
  (interactive "p")
  (let ((default-directory (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                               (and buffer-file-name (file-name-directory buffer-file-name))
                               default-directory)))
    (if (and (executable-find "git") (locate-dominating-file default-directory ".git"))
        (if (executable-find "mob")
            (progn
              (setq mob-default-directory default-directory)
              (if (null minutes)
                  (call-process "mob" nil nil nil "start")
                (call-process "mob" nil nil nil "start" (number-to-string minutes))))
          (message "mob.sh not found. Please install mob.sh and make sure it's in your PATH."))
      (message "Not a git repository. Please make sure you are inside a git repository."))))



(defun highlight-current-line ()
  "Highlight the current line."
  (hl-line-mode 1))

(defun mob-status ()
  "Show the current status of the mob session in a buffer named `*mob-status*`."
  (interactive)
  (let ((buffer (get-buffer-create "*mob-status*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (concat "Current branch: " (car (split-string (shell-command-to-string "mob branch") "\n"))))
      (newline)
      (call-process "mob" nil t nil "status")
      (goto-char (point-min))
      (local-set-key (kbd "q") #'kill-buffer-and-window)
      (when (fboundp 'evil-local-set-key)
        (evil-local-set-key 'normal (kbd "q") #'kill-buffer-and-window)))
    (pop-to-buffer buffer)
    (highlight-current-line)))
