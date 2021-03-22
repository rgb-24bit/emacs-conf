;;; core-defaults.el --- init default config -*- lexical-binding: t; -*-

;;; Code:

;; romove warning ring
(setq ring-bell-function 'ignore)

;; always move to help window
(setq help-window-select t)

;; https://stackoverflow.com/questions/1128927/how-to-scroll-line-by-line-in-gnu-emacs/16229080#16229080
(with-eval-after-load 'view
  (progn
    (define-key view-mode-map (kbd "n") 'scroll-up-line)
    (define-key view-mode-map (kbd "p") 'scroll-down-line)))
(add-hook 'find-function-after-hook 'view-mode)

(provide 'core-defaults)
;;; core-defaults.el ends here
