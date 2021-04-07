;;; init-lsp.el --- init lsp config -*- lexical-binding: t; -*-

;;; Code:

(defcustom emacsc-enable-lsp nil
  "Whether to use lsp, default is `nil'.Can be set by buffer local or dired local.")

(use-package nox
  :commands (nox-ensure)
  :init
  (progn
    (defun emacsc//setup-lsp ()
      "setup the lsp support, when `emacsc-enable-lsp' is t."
      (when emacsc-enable-lsp
        (nox-ensure)))

    (dolist (hook (list
                   'rust-mode-hook
                   'go-mode-hook))
      (add-hook hook 'emacsc//setup-lsp)))
  :config
  (progn
    (general-def nox-mode-map
      "S-<f6>" 'nox-rename
      "C-M-l"  'nox-format)))

(provide 'init-lsp)
;;; init-lsp.el ends here
