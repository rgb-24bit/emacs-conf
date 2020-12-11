;;; init-yasnippet.el --- init yasnippet config -*- lexical-binding: t; -*-

;;; Code:

(use-package yasnippet
  :commands (yas-global-mode yas-minor-mode yas-activate-extra-mode)
  :init
  (progn
    (setq yas-snippet-dirs
          '((expand-file-name "snippets" user-emacs-directory))))
  :config
  (progn
    (add-hook 'prog-mode-hook 'yas-minor-mode)
    (add-hook 'org-mode-hook 'yas-minor-mode)))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
