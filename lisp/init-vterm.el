;;; init-vterm.el --- init vterm config -*- lexical-binding: t; -*-

;;; Code:

(use-package vterm
  :defer t
  :commands (vterm vterm-other-window)
  :init (emacsc-leader-def "'" 'vterm-other-window)
  :config
  (progn
    (setq vterm-shell shell-file-name)

    (define-key vterm-mode-map (kbd "M-n") 'vterm-send-down)
    (define-key vterm-mode-map (kbd "M-p") 'vterm-send-up)
    (define-key vterm-mode-map (kbd "M-y") 'vterm-yank-pop)
    (define-key vterm-mode-map (kbd "M-/") 'vterm-send-tab)

    (defun emacsc//disable-hl-line-mode ()
      "Locally disable global-hl-line-mode"
      (interactive)
      (setq-local global-hl-line-mode nil))

    (add-hook 'vterm-mode-hook 'emacsc//disable-hl-line-mode)))

(provide 'init-vterm)
;;; init-vterm.el ends here
