;;; init-ledger.el --- init ledger config -*- lexical-binding: t; -*-

;;; Code:

(use-package ledger-mode
  :mode ("\\.\\(ledger\\|ldg\\)\\'" . ledger-mode)
  :defer t
  :init
  (progn
    (setq ledger-post-amount-alignment-column 62)

    (add-hook 'ledger-mode-hook
              (lambda () (setq-local pcomplete-termination-string "")))

    ;; ledger-reports config
    (setq ledger-reports
          '(("bal" "%(binary) -f %(ledger-file) bal")
            ("cashflow" "%(binary) -f %(ledger-file) bal ^Income ^Expenses")
            ("networth" "%(binary) -f %(ledger-file) bal ^Assets ^Liabilities")
            ("monthly" "ledger -f %(ledger-file) bal --period %(month)")
            ;; ("weekly" "%(binary) -f %(ledger-file) bal --weekly")
            ("reg" "ledger -f %(ledger-file) reg --date-format %Y/%m/%d")
            ("payee" "ledger -f %(ledger-file) reg @%(payee) --date-format %Y/%m/%d")
            ("account" "ledger -f %(ledger-file) reg %(account) --date-format %Y/%m/%d")))))

  (provide 'init-ledger)

;;; init-ledger.el ends here
