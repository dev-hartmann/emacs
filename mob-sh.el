;;(require transient)

(transient-define-prefix mob ()
  "Wrapper around the mob.sh command line tool."
  ["Base commands"
   ("s" "start" mob-start "Start a mob session")   ("n" "next" mob-next "Pass the keyboard to the next person")
   ("e" "end" mob-end "End the current mob session")
   ("c" "clean" mob-clean "removes local and remote wip branch")
   ("r" "reset" mob-reset "Removes all orphan wip branches")]
  ["Timer commands"
   ("s" "start" mob-start "Start a mob session")
   ("o" "next" mob-next "Pass the keyboard to the next person")
   ("b" "end" mob-end "End the current mob session")]
  ["Options"
   ("-t" "time" mob-set-time "Set the session time (in minutes)" :type 'numeric)])

(defvar mob-time nil
  "The length of the mob session in minutes.")

(defvar mob-output-buffer-name "*mob output*"
  "Name of buffer used to display mob output.")

(defun mob-start ()
  "Start a new mob session."
  (interactive)
  (setq mob-action "start")
  (mob-set-time (read-from-minibuffer "Time (in minutes): ")))

(defun mob-next ()
  "Pass the keyboard to the next person."
  (interactive)
  (setq mob-action "next")
  (mob-run))

(defun mob-end ()
  "End the current mob session."
  (interactive)
  (setq mob-action "end")
  (mob-run))

(defun mob-status ()
  "Show the current mob status."
  (interactive)
  (setq mob-action "status")
  (mob-run))

(defun mob-run ()
  "Run the mob command with the current action and time settings."
  (interactive)
  (let* ((mob-cmd (concat "mob.sh "
                          (mapconcat #'shell-quote-argument
                                     (list
                                      (concat "--time=" (number-to-string (string-to-number mob-time)))
                                      mob-action)
                                     " ")))
         (mob-output-buffer (get-buffer-create mob-output-buffer-name)))
    (with-current-buffer mob-output-buffer
      (setq-local mode-line-format nil)
      (erase-buffer))
    (async-shell-command mob-cmd mob-output-buffer)
    (display-buffer-in-side-window mob-output-buffer '((side . bottom) (slot . -1)))
    (with-current-buffer mob-output-buffer
      (goto-char (point-max)))))
