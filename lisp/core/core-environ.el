;;; core-environ.el --- load special environ config -*- lexical-binding: t; -*-

;;; Code:

(defconst emacsc-environ-file (expand-file-name "init-environ.el" user-emacs-directory))

(defun emacsc/load-environ-config ()
  "Load special environ config."
  (interactive)
  (when (file-exists-p emacsc-environ-file)
    (load emacsc-environ-file)))

(defun emacsc/open-environ-config ()
  "Open special environ config file."
  (interactive)
  (find-file-existing emacsc-environ-file))

(defun emacsc/load-init-config ()
  "Load user init file."
  (interactive)
  (when (file-exists-p user-init-file)
    (load user-init-file)))

(defun emacsc/open-init-file ()
  "Open user init file."
  (interactive)
  (find-file-existing user-init-file))

(provide 'core-environ)
;;; core-environ.el ends here
