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

(provide 'core-defaults)
;;; core-defaults.el ends here
