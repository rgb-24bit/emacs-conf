;;; init-rust.el --- init rust config -*- lexical-binding: t; -*-

;;; Code:

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (emacsc-leader-def rust-mode-map
    "m"     '(:ignore t :which-key "rust")

    "m ="   '(:ignore t :which-key "format")
    "m = =" 'rust-format-buffer))

(provide 'init-rust)
;;; init-rust.el ends here
