;;; init-lsp.el --- init lsp config -*- lexical-binding: t; -*-

;;; Code:

(defcustom emacsc-enable-lsp nil
  "Whether to use lsp, default is `nil'. Can be set by buffer local or dired local.")

(defcustom emacsc-lsp-client 'nox
  "Specify the lsp client used by emacs."
  :type '(choice
          (const :tag "nox" nox)
          (const :tag "lsp-mode" lsp-mode)))

(use-package-straight posframe
  :defer t)

;; =============================================================================
;; nox
;; =============================================================================

(use-package nox
  :commands (nox-ensure)
  :config
  (progn
    (general-def nox-mode-map
      "S-<f6>" 'nox-rename
      "C-M-l"  'nox-format
      "C-h ."  'nox-show-doc)))

;; =============================================================================
;; lsp-mode
;; =============================================================================

(use-package-straight lsp-mode
  :init
  (setq lsp-keymap-prefix "M-m m l")
  :hook
  ((lsp-mode . lsp-enable-which-key-integration))
  :commands
  (lsp lsp-deferred)
  :config
  (setq lsp-enable-symbol-highlighting nil
        lsp-lens-enable nil
        lsp-completion-provider :none
        lsp-modeline-code-actions-enable nil)
  (general-def lsp-mode-map
    "S-<f6>" 'lsp-rename
    "C-M-l"  'lsp-format-buffer
    "C-h ."  'lsp-describe-thing-at-point
    "M-RET"  'lsp-execute-code-action)
  (define-key lsp-mode-map (kbd "TAB") nil))

(use-package-straight lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-delay 2)
  (general-def lsp-ui-mode-map
    "C-x ." 'lsp-ui-doc-focus-frame))

;; =============================================================================
;; flycheck
;; =============================================================================

(use-package-straight flycheck
  :defer t
  :config
  (setq flycheck-idle-change-delay 2)
  (general-def flycheck-mode-map
    "<f2>" 'flycheck-next-error))

;; =============================================================================
;; lsp setup
;; =============================================================================

(defun emacsc//setup-lsp ()
  "setup the lsp support, when `emacsc-enable-lsp' is t."
  (when emacsc-enable-lsp
    (cl-case emacsc-lsp-client
      (nox (nox-ensure))
      (lsp-mode (lsp-deferred))
      (t (message "unsupported lsp client %s" emacsc-lsp-client)))))

(dolist (hook (list
               'rust-mode-local-vars-hook
               'go-mode-local-vars-hook))
  (add-hook hook 'emacsc//setup-lsp))

(provide 'init-lsp)
;;; init-lsp.el ends here
