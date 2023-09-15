;;;; custom-functions.el ---- collection of custom fns
;;;; Code:


(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(provide 'custom-functions)
 
