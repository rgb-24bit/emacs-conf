;;; init-completion.el --- init completion config -*- lexical-binding: t; -*-

;;; Code:

(use-package-straight company
  :commands global-company-mode
  :init
  (progn
    (setq company-idle-delay 0.2
          company-minimum-prefix-length 1
          company-require-match nil
          company-dabbrev-ignore-case nil
          company-show-numbers t
          company-dabbrev-downcase nil)
    (setq company-global-modes '(not lsp-bridge-mode)))
  (add-hook 'after-init-hook (global-company-mode))
  :config
  (defun company-mode/backend-with-yas (backend)
    (if (and (listp backend) (member 'company-yasnippet backend))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(provide 'init-completion)
;;; init-completion.el ends here
