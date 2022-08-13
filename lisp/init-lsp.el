;;; init-lsp.el --- init lsp config -*- lexical-binding: t; -*-

;;; Code:

(defcustom emacsc-enable-lsp nil
  "Whether to use lsp, default is `nil'. Can be set by buffer local or dired local.")

(use-package-straight posframe
  :defer t)

;; =============================================================================
;; lsp-bridge
;; =============================================================================

(use-package-straight lsp-bridge)

;; (global-lsp-bridge-mode)

;; acm conflict with company
(add-hook 'acm-mode-hook '(lambda () (company-mode -1)))

(general-def lsp-bridge-mode-map
  "M-,"    'lsp-bridge-return-from-def
  "M-."    'lsp-bridge-find-def
  "M-?"    'lsp-bridge-find-references
  "S-<f6>" 'lsp-bridge-rename)

;; =============================================================================
;; flycheck
;; =============================================================================

(use-package-straight flycheck
  :defer t
  :config
  (setq flycheck-check-syntax-automatically '(save)
        flycheck-idle-change-delay 3)
  (general-def flycheck-mode-map
    "<f2>" 'flycheck-next-error))

;; =============================================================================
;; lsp setup
;; =============================================================================

(defun emacsc//setup-lsp ()
  "setup the lsp support, when `emacsc-enable-lsp' is t."
  (when emacsc-enable-lsp
    (lsp-bridge-mode)))

(dolist (hook (list
               'emacs-lisp-mode-local-vars-hook
               'rust-mode-local-vars-hook
               'go-mode-local-vars-hook
               'python-mode-local-vars-hook
               'js-mode-local-vars-hook))
  (add-hook hook 'emacsc//setup-lsp))

(provide 'init-lsp)
;;; init-lsp.el ends here
