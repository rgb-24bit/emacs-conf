;;; init-vc.el --- init version control config -*- lexical-binding: t; -*-

;;; Code:

(use-package magit
  :commands (magit-status magit-list-repositories)
  :init
  (emacsc-leader-def
    "g"  '(:ignore t :which-key "git")
    "gs" 'magit-status
    "gL" 'magit-list-repositories)
  :config
  (progn
    (when-let ((git (executable-find "git")))
      (setq magit-git-executable git))

    (when emacsc-system-is-windows
      (setq magit-commit-show-diff nil))

    ;; https://emacs.stackexchange.com/questions/32696/how-to-use-magit-list-repositories
    (setq magit-repolist-columns
	  '(("S"        1 magit-repolist-column-flag                   ())
	    ("L>U"      3 magit-repolist-column-unpushed-to-upstream   ((:right-align t)))
	    ("Name"    25 magit-repolist-column-ident                  ())
	    ("Branch"  20 magit-repolist-column-branch                 ())
	    ("Path"    99 magit-repolist-column-path                   ())))

    (setq magit-repolist-column-flag-alist
	  '((magit-untracked-files . "N")
	    (magit-unstaged-files  . "M")
	    (magit-staged-files    . "M")))))

(provide 'init-vc)
;;; init-vc.el ends here
