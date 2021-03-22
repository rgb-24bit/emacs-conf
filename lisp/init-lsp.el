;;; init-lsp.el --- init lsp config -*- lexical-binding: t; -*-

;;; Code:

(use-package nox
  :commands (nox-ensure)
  :config
  (progn
    (general-def nox-mode-map
      "S-<f6>" 'nox-rename
      "C-M-l"  'nox-format)))

(provide 'init-lsp)
;;; init-lsp.el ends here
