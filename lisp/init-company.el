;;; init-company.el --- init company mode config -*- lexical-binding: t; -*-

;;; Code:

(use-package company
  :commands global-company-mode
  :init
  (progn
    (setq company-idle-delay 0.2
          company-minimum-prefix-length 2
          company-require-match nil
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil))
  (add-hook 'after-init-hook (global-company-mode)))

(provide 'init-company)
;;; init-company.el ends here
