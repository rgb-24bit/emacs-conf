;;; init.el --- init emacs config -*- lexical-binding: t -*-

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

;; =============================================================================
;; define base lisp dirctory
;; =============================================================================

(defconst emacsc-lisp-directory (expand-file-name "lisp" user-emacs-directory))
(defconst emacsc-core-directory (expand-file-name "core" emacsc-lisp-directory))

(add-to-list 'load-path emacsc-lisp-directory)
(add-to-list 'load-path emacsc-core-directory)

;; =============================================================================
;; Adjust garbage collection thresholds during startup, and thereafter
;; =============================================================================

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;; init.el ends here
