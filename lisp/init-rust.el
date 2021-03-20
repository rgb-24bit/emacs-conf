;;; init-rust.el --- init rust config -*- lexical-binding: t; -*-

;;; Code:

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil))))

(provide 'init-rust)
;;; init-rust.el ends here
