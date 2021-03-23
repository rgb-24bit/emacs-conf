;;; init-rust.el --- init rust config -*- lexical-binding: t; -*-

;;; Code:

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil)))
  (defun emacsc//rust-setup-lsp ()
    (when (and (boundp 'emacsc-enable-lsp) emacsc-enable-lsp)
      (nox-ensure)))
  (add-hook 'rust-mode-hook 'emacsc//rust-setup-lsp))

(provide 'init-rust)
;;; init-rust.el ends here
