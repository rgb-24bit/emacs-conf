;;; init-rust.el --- init rust config -*- lexical-binding: t; -*-

;;; Code:

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil)))
  (add-hook 'rust-mode-hook 'nox-ensure))

(provide 'init-rust)
;;; init-rust.el ends here
