;;; init-rime.el --- init rime config -*- lexical-binding: t; -*-

;;; Code:

(use-package-straight rime)

(setq rime-user-data-dir "~/.config/fcitx/rime")

(setq default-input-method "rime"
      rime-show-candidate 'posframe
      rime-posframe-style 'vertical)

(setq rime-inline-ascii-trigger 'shift-l)

(setq rime-disable-predicates
      '(rime-predicate-after-ascii-char-p
        rime-predicate-prog-in-code-p
        rime-predicate-punctuation-line-begin-p
        rime-predicate-space-after-cc-p
        rime-predicate-current-uppercase-letter-p))
(define-key rime-mode-map (kbd "M-j") 'rime-force-enable)

(provide 'init-rime)
;;; init-rime.el ends here
