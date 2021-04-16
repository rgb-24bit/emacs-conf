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

(defun emacsc/toggle-frame-fullscreen (&optional frame)
  "Toggle fullscreen state of FRAME.
Make selected frame fullscreen or restore its previous size
if it is already fullscreen.

Before making the frame fullscreen remember the current value of
the frame's `fullscreen' parameter in the `fullscreen-restore'
parameter of the frame.  That value is used to restore the
frame's fullscreen state when toggling fullscreen the next time.

Note that with some window managers you may have to set
`frame-resize-pixelwise' to non-nil in order to make a frame
appear truly fullscreen.  In addition, you may have to set
`x-frame-normalize-before-maximize' in order to enable
transitions from one fullscreen state to another.

See also `toggle-frame-maximized'."
  (interactive)
  (let ((fullscreen (frame-parameter frame 'fullscreen)))
    (if (memq fullscreen '(fullscreen fullboth))
	(let ((fullscreen-restore (frame-parameter frame 'fullscreen-restore)))
	  (if (memq fullscreen-restore '(maximized fullheight fullwidth))
	      (set-frame-parameter frame 'fullscreen fullscreen-restore)
	    (set-frame-parameter frame 'fullscreen nil)))
      (modify-frame-parameters
       frame `((fullscreen . fullboth) (fullscreen-restore . ,fullscreen))))
    ;; Manipulating a frame without waiting for the fullscreen
    ;; animation to complete can cause a crash, or other unexpected
    ;; behavior, on macOS (bug#28496).
    (when (featurep 'cocoa) (sleep-for 0.5))))

(defun emacsc/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
	(buffer (current-buffer))
	(name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
	(ido-kill-buffer)
      (if (yes-or-no-p
	   (format "Are you sure you want to delete this file: '%s'?" name))
	  (progn
	    (delete-file filename t)
	    (kill-buffer buffer)
	    (message "File deleted: '%s'" filename))
	(message "Canceled: File deletion")))))

(defun emacsc/jump-prev-func (&optional arg)
  "jump to previous function by `beginning-of-defun'."
  (interactive "^p")
  (beginning-of-defun arg))

(defun emacsc/jump-next-func (&optional arg)
  "jump to previous function by `beginning-of-defun'."
  (interactive "^p")
  (beginning-of-defun (* -1 arg)))

(provide 'core-funcs)
;;; core-funcs.el<lisp> ends here
