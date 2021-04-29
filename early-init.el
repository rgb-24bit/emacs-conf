;;; early-init.el --- early init -*- lexical-binding: t; -*-

;;; Code:

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. straight.el handles package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;;; early-init.el ends here
