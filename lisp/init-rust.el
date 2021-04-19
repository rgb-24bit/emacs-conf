;;; init-rust.el --- init rust config -*- lexical-binding: t; -*-

;;; Code:

(use-package-straight rust-mode
  :mode "\\.rs\\'"
  :config
  ;; keep global `C-c C-n' binding
  (define-key rust-mode-map (kbd "C-c C-n") nil)

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
