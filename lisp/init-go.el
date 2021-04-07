;;; init-go.el --- init golang config -*- lexical-binding: t; -*-

;;; Code:

(use-package go-mode
  :mode "\\.go\\'"
  :config
  (emacsc-leader-def go-mode-map
    "m"     '(:ignore t :which-key "golang")

    "m ="   '(:ignore t :which-key "format")
    "m = =" 'gofmt))

(provide 'init-go)
;;; init-go.el ends here
