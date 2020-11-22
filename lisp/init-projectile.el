;;; init-projectile.el --- init projectile config -*- lexical-binding: t; -*-

;;; Code:

(use-package projectile
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
    (when (and emacsc/system-is-windows (executable-find "find")
               (not (file-in-directory-p
                     (executable-find "find") "C:\\Windows")))
      (setq  projectile-indexing-method 'alien
             projectile-generic-command "find . -type f"))
    (setq projectile-sort-order 'recentf
          projectile-cache-file (concat emacsc-cache-directory
                                        "projectile.cache")
          projectile-known-projects-file (concat emacsc-cache-directory
                                                 "projectile-bookmarks.eld")))
  :config
  (projectile-mode))

(provide 'init-projectile)
;;; init-projectile.el ends here
