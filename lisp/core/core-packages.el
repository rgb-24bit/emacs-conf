;;; core-packages.el --- init package manager -*- lexical-binding: t; -*-

;;; Code:

(require 'package)
(require 'cl-lib)

;; =============================================================================
;; init package.el
;; =============================================================================

;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(setq package-user-dir
      (expand-file-name (format "elpa/%s.%s" emacs-major-version emacs-minor-version)
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

(package-initialize)

;; =============================================================================
;; init site lisp
;; =============================================================================

(defun emacsc/add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (mapcar
   (lambda (path) (add-to-list 'load-path path))
   (delete-dups (mapcar 'file-name-directory (directory-files-recursively dir "\.el$")))))

(defun emacsc/add-site-lisp-to-load-path ()
  (interactive)
  "Add both site-lisp and its immediate subdirs to `load-path'."
  (emacsc/add-subdirs-to-load-path (expand-file-name "site-lisp/" user-emacs-directory)))

(emacsc/add-site-lisp-to-load-path)

;; =============================================================================
;; init straight
;; =============================================================================

(setq straight-repository-branch "develop"
      straight-build-dir (format "build-%s" emacs-version)
      straight-check-for-modifications nil
      straight-enable-package-integration nil
      straight-use-package-by-default nil
      straight-vc-git-default-clone-depth 1)

(require 'straight-bootstrap)

(defconst emacsc-recipes-directory (expand-file-name "straight/recipes" user-emacs-directory)
  "Emacs config recipes directory.")

(defun straight-recipes-emacsc-retrieve (package)
  "Look up a PACKAGE recipe in emacsc-recipes."
  (with-temp-buffer
    (condition-case nil
        (progn
          (insert-file-contents-literally
           (expand-file-name (symbol-name package) emacsc-recipes-directory))
          (read (current-buffer)))
      (error nil))))

(defun straight-recipes-emacsc-list ()
  "Return a list of recipes available in EMACSC, as a list of strings."
  (straight--directory-files emacsc-recipes-directory "^[^.]"))

(defun straight-recipes-emacsc-version ()
  "Return the current version of the EMACSC retriever."
  1)

(straight-use-recipes '(emacsc :type built-in :build nil))

;; make emacsc has higher priority
(setq straight-recipe-repositories
      '(emacsc org-elpa melpa gnu-elpa-mirror el-get emacsmirror-mirror))

;; =============================================================================
;; init use-package
;; =============================================================================

(require 'use-package)

(defmacro use-package-straight (name &rest args)
  "A simple `use-package' wrapper, auto add (:straight t) argument.
Because the `straight-use-package' is no highlight."
  (declare (indent 1))
  `(use-package ,name ,@(nconc args '(:straight t))))

(provide 'core-packages)
;;; core-packages.el ends here
