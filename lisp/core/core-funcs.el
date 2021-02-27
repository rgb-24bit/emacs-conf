;;; core-funcs.el<lisp> --- core funcs -*- lexical-binding: t; -*-

;;; Code:

(defun emacsc/recompile-packages (dir &optional force)
  "Compile or recompile packages in given directory.
This function compiles all `.el' files in the given directory
if it's corresponding `.elc' file is missing or outdated.

This is useful if you switch Emacs versions or there
are issues with a local package which require a recompile.

If FORCE is non-nil, force recompile of all found `.el' files."
  (byte-recompile-directory dir 0 force))

(defun emacsc/recompile-site-lisp-packages ()
  "Compile or recompile site-lisp packages."
  (interactive)
  (emacsc/recompile-packages
   (expand-file-name "site-lisp" user-emacs-directory) t))

(defun emacsc/recompile-elpa-packages ()
  "Compile or recompile elpa packages."
  (interactive)
  (emacsc/recompile-packages package-user-dir t))

(defun emacsc/load-init-config ()
  "Load user init file."
  (interactive)
  (when (file-exists-p user-init-file)
    (load user-init-file)))

(defun emacsc/open-init-file ()
  "Open user init file."
  (interactive)
  (find-file-existing user-init-file))

(defun emacsc/alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (cl-destructuring-bind (buf start pos)
      (cl-find (window-buffer window) (window-prev-buffers)
	       :key #'car :test-not #'eq)
    (list (other-buffer) nil nil)
    (if (not buf)
	(message "Last buffer not found.")
      (set-window-buffer-start-and-point window buf start pos))))

(defun emacsc/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
	(progn
	  (indent-region (region-beginning) (region-end))
	  (message "Indented selected region."))
      (progn
	(indent-region (point-min) (point-max))
	(message "Indented buffer.")))
    (whitespace-cleanup)))

(defun emacsc/kill-this-buffer (&optional arg)
  "Kill the current buffer."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (kill-buffer)))

(provide 'core-funcs)
;;; core-funcs.el<lisp> ends here
