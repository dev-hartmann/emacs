;;;; custom-functions.el ---- collection of custom fns
;;;; Code:

(defun dh/create-vertical-window-and-projectile-find ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (consult-projectile-find-file)
  (balance-windows))

(defun dh/close-window-and-balance ()
  (interactive)
  (delete-window)
  (balance-windows))

(defun dh/undo-tree-split-side-by-side (original-function &rest args)
  "Split undo-tree side-by-side"
  (let ((split-height-threshold nil)
        (split-width-threshold 0))
    (apply original-function args)))

(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(provide 'custom-functions)
 
