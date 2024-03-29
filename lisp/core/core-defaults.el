;;; core-defaults.el --- init default config -*- lexical-binding: t; -*-

;;; Code:

;; romove warning ring
(setq ring-bell-function 'ignore
      visible-bell nil)

;; always move to help window
(setq help-window-select t)

;; https://stackoverflow.com/questions/1128927/how-to-scroll-line-by-line-in-gnu-emacs/16229080#16229080
(with-eval-after-load 'view
  (progn
    (define-key view-mode-map (kbd "n") 'scroll-up-line)
    (define-key view-mode-map (kbd "p") 'scroll-down-line)))
(add-hook 'find-function-after-hook 'view-mode)

;; typed text replaces the selection if the selection is active
(delete-selection-mode)

;; Highlight and allow to open http link at point in programming buffers
;; goto-address-prog-mode only highlights links in strings and comments
(add-hook 'prog-mode-hook 'goto-address-prog-mode)
;; Highlight and follow bug references in comments and strings
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)

;; use only spaces and no tabs
(setq-default indent-tabs-mode nil
              tab-width 2)

;; Use system trash for file deletion.
;; This should work on Windows and Linux distros.
(setq delete-by-moving-to-trash t)

;; when t, 27.2 will ignores modifier keys when IME input is used.
(setq w32-ignore-modifiers-on-IME-input nil)

;; explicitly set the preferred coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(setq-default buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; auto revert
(global-auto-revert-mode)

;; hook into `hack-local-variables' in order to allow switching spacemacs
;; configurations based on local variables
(add-hook 'hack-local-variables-hook #'emacsc//run-local-vars-mode-hook)

;; close native comp warning
(setq native-comp-async-report-warnings-errors nil)

;; scratch buffer
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; =============================================================================
;; wsl browser
;; https://emacs-china.org/t/wsl-emacs-windows/18605/2
;; =============================================================================

(defun emacsc//browse-url-generic (url &optional _new-window)
  ;; new-window ignored
  "Ask the WWW browser defined by `browse-url-generic-program' to load URL.
Default to the URL around or before point.  A fresh copy of the
browser is started up in a new process with possible additional arguments
`browse-url-generic-args'.  This is appropriate for browsers which
don't offer a form of remote control."
  (interactive (browse-url-interactive-arg "URL: "))
  (if (not browse-url-generic-program)
      (error "No browser defined (`browse-url-generic-program')"))
  (apply 'call-process browse-url-generic-program nil
	 0 nil
	 (append browse-url-generic-args
                 (list (format "start %s"
                               (replace-regexp-in-string "&" "^&" url))))))

(when (and (eq system-type 'gnu/linux)
           (string-match
            "Linux.*Microsoft.*Linux"
            (shell-command-to-string "uname -a")))
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c")
   browse-url-browser-function #'emacsc//browse-url-generic))

(provide 'core-defaults)
;;; core-defaults.el ends here
