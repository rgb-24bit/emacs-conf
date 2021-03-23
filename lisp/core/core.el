;;; core.el --- basic config & load core lisp code -*- lexical-binding: t; -*-

;;; Code:

(when (and emacsc-system-is-windows (null (getenv "HOME")))
  (setenv "HOME" (getenv "USERPROFILE")))

;; =============================================================================
;; load core code
;; =============================================================================

(require 'core-packages)
(require 'core-paths)
(require 'core-funcs)
(require 'core-themes)
(require 'core-keybindings)
(require 'core-defaults)

(provide 'core)
;;; core.el ends here
