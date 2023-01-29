(defun mob-available-p ()
  "Tests if the `mob` command is available."
  (eq 0 (call-process "which" nil nil nil "mob")))

(defun mob-handle-error (err output)
  (with-current-buffer (get-buffer-create "*mob-error*")
    (erase-buffer)
    (insert (if err (error-message-string err) output))
    (pop-to-buffer (current-buffer))))

(defun mob-start (&optional minutes)
  "Start a new mob session with optional timer of MINUTES."
  (interactive "p")
  (let ((default-directory (or (and (fboundp 'projectile-project-root) (projectile-project-root))
                               (and buffer-file-name (file-name-directory buffer-file-name))
                               default-directory)))
    (if (locate-dominating-file default-directory ".git")
        (condition-case err
            (let ((output (shell-command-to-string (concat "mob start " (if minutes (number-to-string minutes))))))
              (unless (string-match-p "> It's now.*Happy collaborating! :)" output)
                (mob-handle-error nil output))))
      (mob-handle-error nil "Not a git project"))))


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
