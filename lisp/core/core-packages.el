;;; core-packages.el --- init package manager -*- lexical-binding: t; -*-

;;; Code:

(require 'package)
(require 'cl-lib)

;; =============================================================================
;; init package.el
;; =============================================================================

;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                        user-emacs-directory))

;; ELPA Mirror Settings (Mirror Source for Emacs China)
(setq package-archives
      '(("melpa-tuna" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("gnu-tuna"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("org-cn"     . "http://elpa.emacs-china.org/org/")
        ("gnu-cn"     . "http://elpa.emacs-china.org/gnu/")))

;; Work-around for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(when (and (version< emacs-version "26.3") (boundp 'libgnutls-version) (>= libgnutls-version 30604))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; =============================================================================
;; init site lisp
;; =============================================================================

(defun emacsc/add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir))
        (orig-load-path load-path))
    (setq load-path (cons dir nil))
    (normal-top-level-add-subdirs-to-load-path)
    (nconc load-path orig-load-path)))

(defun emacsc/add-site-lisp-to-load-path ()
  "Add both site-lisp and its immediate subdirs to `load-path'."
  (emacsc/add-subdirs-to-load-path (expand-file-name "site-lisp/" user-emacs-directory)))

(emacsc/add-site-lisp-to-load-path)

;; =============================================================================
;; init use-package
;; =============================================================================

(require 'use-package)

(provide 'core-packages)
;;; core-packages.el ends here
