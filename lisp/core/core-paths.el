;;; core-paths.el --- define base paths -*- lexical-binding: t; -*-

;;; Code:

(defconst emacsc-cache-directory (expand-file-name ".cache" user-emacs-directory))
(defconst emacsc-bin-directory (expand-file-name "bin" user-emacs-directory))

(when (memq window-system '(mac ns x))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize))

(provide 'core-paths)
;;; core-paths.el ends here
