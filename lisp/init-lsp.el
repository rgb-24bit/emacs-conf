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

(use-package-straight nox
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
        lsp-modeline-code-actions-enable nil
        lsp-auto-execute-action nil)
  (general-def lsp-mode-map
    "S-<f6>" 'lsp-rename
    "C-M-l"  'lsp-format-buffer
    "C-h ."  'lsp-describe-thing-at-point
    "M-RET"  'lsp-execute-code-action)
  (define-key lsp-mode-map (kbd "TAB") nil))

(use-package-straight lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-update-mode 'line
        lsp-ui-sideline-show-hover t
        lsp-ui-doc-enable nil)
  (general-def lsp-ui-mode-map
    "C-x ." 'lsp-ui-doc-focus-frame)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

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
    (cl-case emacsc-lsp-client
      (nox (nox-ensure))
      (lsp-mode (lsp-deferred))
      (t (message "unsupported lsp client %s" emacsc-lsp-client)))))

(dolist (hook (list
               'rust-mode-local-vars-hook
               'go-mode-local-vars-hook
               'python-mode-local-vars-hook
               'js-mode-local-vars-hook))
  (add-hook hook 'emacsc//setup-lsp))

(provide 'init-lsp)
;;; init-lsp.el ends here
