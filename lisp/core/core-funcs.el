;;; core-funcs.el --- core funcs -*- lexical-binding: t; -*-

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
      (progn (cl-find (window-buffer window) (window-prev-buffers)
                      :key #'car :test-not #'eq)
             (list (other-buffer) nil nil))
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

;; https://github.com/syl20bnr/spacemacs/blob/develop/layers/+spacemacs/spacemacs-defaults/funcs.el#L26
(defun emacsc//run-local-vars-mode-hook ()
  "Run a hook for the major-mode after the local variables have been processed."
  (run-hooks (intern (format "%S-local-vars-hook" major-mode))))

(defun emacsc/dos2unix ()
  "Converts the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun emacsc/unix2dos ()
  "Converts the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

(defun emacsc/copy-file ()
  "Write the file under new name."
  (interactive)
  (call-interactively 'write-file))

;; from https://www.emacswiki.org/emacs/CopyingWholeLines
(defun emacsc/duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ; Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      ;; Go to beginning of next line, or make a new one
                      (if (< 0 (forward-line 1))
                          (newline))))))
        (dotimes (i (abs (or n 1)))   ; Insert N times, or once if not specified
          (insert text))))
    (if use-region nil        ; Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ; Save column
        (if (> 0 n)                     ; Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

;; https://github.com/syl20bnr/spacemacs/blob/master/layers/+distributions/spacemacs-base/funcs.el#L294
(defun emacsc/rename-file-and-buffer ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let* ((dir (file-name-directory filename))
             (new-name (read-file-name "New name: " dir)))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (let ((dir (file-name-directory new-name)))
                 (when (and (not (file-exists-p dir)) (yes-or-no-p (format "Create directory '%s'?" dir)))
                   (make-directory dir t)))
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (when (fboundp 'recentf-add-file)
                 (recentf-add-file new-name)
                 (recentf-remove-if-non-kept filename))
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

(defun emacsc/adjust-frame-opacity (incr)
  "Adjust the background opacity of by increment INCR."
  (interactive "^p")
  (unless (display-graphic-p)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter nil 'alpha) 100))
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

(defun emacsc/adjust-frame-opacity-+2 ()
  "Adjust the background opacity of by increment +2."
  (interactive)
  (emacsc/adjust-frame-opacity +2))

(defun emacsc/adjust-frame-opacity--2 ()
  "Adjust the background opacity of by increment -2."
  (interactive)
  (emacsc/adjust-frame-opacity -2))

(defun emacsc/remember-init ()
  "Remember current position and setup."
  (interactive)
  (point-to-register 8)
  (message "Have remember one position"))

(defun emacsc/remember-jump ()
  "Jump to latest position and setup."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp))
  (message "Have back to remember position"))

(defun emacsc/point-stack-push ()
  "Push current point in stack."
  (interactive)
  (message "Location marked.")
  (setq point-stack (cons (list (current-buffer) (point)) point-stack)))

(defun emacsc/point-stack-pop ()
  "Pop point from stack."
  (interactive)
  (if (null point-stack)
      (message "Stack is empty.")
    (switch-to-buffer (caar point-stack))
    (goto-char (cadar point-stack))
    (setq point-stack (cdr point-stack))))

(provide 'core-funcs)
;;; core-funcs.el ends here
