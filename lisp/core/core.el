;;; core.el --- basic config & load core lisp code -*- lexical-binding: t; -*-

;;; Code:

(defconst emacsc/system-is-mac     (eq system-type 'darwin))
(defconst emacsc/system-is-linux   (eq system-type 'gnu/linux))
(defconst emacsc/system-is-windows (memq system-type '(cygwin windows-nt ms-dos)))

(when (and emacsc/system-is-windows (null (getenv "HOME")))
  (setenv "HOME" (getenv "USERPROFILE")))

;; =============================================================================
;; load core code
;; =============================================================================

(require 'core-packages)
(require 'core-themes)
(require 'core-environ)

(provide 'core)
;;; core.el ends here
