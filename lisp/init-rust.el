;;; init-rust.el --- init rust config -*- lexical-binding: t; -*-

;;; Code:

(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (emacsc-leader-def rust-mode-map
    "m"     '(:ignore t :which-key "rust")

    "m ="   '(:ignore t :which-key "format")
    "m = =" 'rust-format-buffer

    "m c"   '(:ignore t :which-key "compile")
    "m c c" 'rust-compile
    "m c d" 'rust-check

    "m t"   '(:ignore t :which-key "test")
    "m t t" 'rust-test
    ))

(provide 'init-rust)
;;; init-rust.el ends here
