;;; init-python.el --- init python config -*- lexical-binding: t; -*-

;;; Code:

(setq python-indent-guess-indent-offset t
      python-indent-guess-indent-offset-verbose nil)

(use-package-straight pyvenv
  :commands (pyvenv-activate pyvenv-deactivate))

(emacsc-leader-def python-mode-map
  "m"     '(:ignore t :which-key "python")
  "m v"   '(:ignore t :which-key "venv")
  "m v a" 'pyvenv-activate
  "m v d" 'pyvenv-deactivate)

(provide 'init-python)
;;; init-python.el ends here
