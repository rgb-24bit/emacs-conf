;;; init-projectile.el --- init projectile config -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

(use-package-straight f)

(defcustom emacsc-ignored-project-directories
  '("~/.cargo/registry" "~/.rustup" "~/go/pkg" "~/.emacs.d/site-lisp" "~/.emacs.d/straight")
  "Ignore projects under these directories."
  :type '(repeat string))

(defun emacsc//ignored-project-function (project-root)
  (cl-dolist (dir emacsc-ignored-project-directories)
    (when (f-descendant-of? project-root (expand-file-name dir))
      (cl-return t))))

(use-package-straight projectile
  :commands (projectile-ack
             projectile-ag
             projectile-compile-project
             projectile-dired
             projectile-find-dir
             projectile-find-file
             projectile-find-tag
             projectile-test-project
             projectile-grep
             projectile-invalidate-cache
             projectile-kill-buffers
             projectile-multi-occur
             projectile-project-p
             projectile-project-root
             projectile-recentf
             projectile-regenerate-tags
             projectile-replace
             projectile-replace-regexp
             projectile-run-async-shell-command-in-root
             projectile-run-shell-command-in-root
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-vc)
  :init
  (progn
    ;; note for Windows: GNU find or Cygwin find must be in path to enable
    ;; fast indexing. Also, because windows ships with a program called
    ;; c:\windows\system32\find.exe that is very much not findutils find
    ;; we ignore that specific executable
    (when (and emacsc-system-is-windows (executable-find "find")
               (not (file-in-directory-p
                     (executable-find "find") "C:\\Windows")))
      (setq projectile-indexing-method 'alien
            projectile-generic-command "find . -type f"))
    (setq projectile-sort-order 'recentf
          projectile-cache-file (expand-file-name "projectile.cache"
                                                  emacsc-cache-directory)
          projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld"
                                                           emacsc-cache-directory)
          projectile-ignored-project-function #'emacsc//ignored-project-function)
    )
  :config
  (progn
    (emacsc-leader-def
      "p"   '(:ignore t :which-key "projects")
      "p f" 'projectile-find-file
      "p d" 'projectile-find-dir
      "p p" 'projectile-switch-project
      "p m" 'projectile-remove-known-project
      "p k" 'projectile-kill-buffers)

    (projectile-mode)))

(provide 'init-projectile)
;;; init-projectile.el ends here
