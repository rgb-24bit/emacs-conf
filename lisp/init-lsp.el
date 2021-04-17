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
      ;; https://stackoverflow.com/questions/5147060/how-can-i-access-directory-local-variables-in-my-major-mode-hooks
      ;; access directory-local variables in major mode hooks
      (add-hook 'hack-local-variables-hook
                (lambda ()
                  (when emacsc-enable-lsp
                    (nox-ensure)))))

    (dolist (hook (list
                   'rust-mode-hook
                   'go-mode-hook))
      (add-hook hook 'emacsc//setup-lsp)))
  :config
  (progn
    (require 'posframe)

    (general-def nox-mode-map
      "S-<f6>" 'nox-rename
      "C-M-l"  'nox-format
      "C-h ."  'nox-show-doc)))

(provide 'init-lsp)
;;; init-lsp.el ends here
