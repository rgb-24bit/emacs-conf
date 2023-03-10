;;; init-rime.el --- init rime config -*- lexical-binding: t; -*-

;;; Code:

(use-package-straight rime)

(setq rime-user-data-dir "~/.config/fcitx/rime")

(setq default-input-method "rime"
      rime-show-candidate 'posframe)

(provide 'init-rime)
;;; init-rime.el ends here
