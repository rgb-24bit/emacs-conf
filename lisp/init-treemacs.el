;;; init-treemacs.el --- init treemacs config -*- lexical-binding: t; -*-

;;; Code:

(use-package-straight treemacs
  :defer t
  :commands (treemacs-select-window treemacs)
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  (emacsc-leader-def "f T" 'treemacs))

(provide 'init-treemacs)
;;; init-treemacs.el ends here
