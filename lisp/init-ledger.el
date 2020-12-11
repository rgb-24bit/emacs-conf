;;; init-ledger.el --- init ledger config -*- lexical-binding: t; -*-

;;; Code:

(use-package ledger-mode
  :mode ("\\.\\(ledger\\|ldg\\)\\'" . ledger-mode)
  :defer t
  :init
  (progn
    (setq ledger-post-amount-alignment-column 62)
    (add-hook 'ledger-mode-hook
              (lambda () (setq-local pcomplete-termination-string "")))))

(provide 'init-ledger)

;;; init-ledger.el ends here
