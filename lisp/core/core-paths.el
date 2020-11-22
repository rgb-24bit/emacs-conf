;;; core-paths.el --- define base paths -*- lexical-binding: t; -*-

;;; Code:

(defconst emacsc-cache-directory (expand-file-name ".cache" user-emacs-directory))
(defconst emacsc-bin-directory (expand-file-name "bin" user-emacs-directory))

(provide 'core-paths)
;;; core-paths.el ends here
