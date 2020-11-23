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

(provide 'core-funcs)
;;; core-funcs.el<lisp> ends here
