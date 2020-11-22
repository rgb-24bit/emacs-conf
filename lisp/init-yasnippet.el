;;; init-yasnippet.el --- init yasnippet config -*- lexical-binding: t; -*-

;;; Code:

(use-package yasnippet
    :commands (yas-global-mode yas-minor-mode yas-activate-extra-mode)
    :init
    (progn
      (setq yas-snippet-dirs
            '((expand-file-name "snippets" user-emacs-directory)))

;;; init-yasnippet.el ends here
