;;; init-go.el --- init golang config -*- lexical-binding: t; -*-

;;; Code:

(use-package go-mode
  :mode "\\.go\\'"
  :config
  (defun emacsc//go-setup-lsp ()
    (when (and (boundp 'emacsc-enable-lsp) emacsc-enable-lsp)
      (nox-ensure)))
  (add-hook 'go-mode-hook 'emacsc//go-setup-lsp))

(provide 'init-go)
;;; init-go.el ends here
