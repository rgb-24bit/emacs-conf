;;; core-themes.el --- emacs theme config -*- lexical-binding: t; -*-

;;; Code:

;; =============================================================================
;; init themes
;; =============================================================================

(require 'color-theme-sanityinc-solarized)

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(sanityinc-solarized-dark))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

;; =============================================================================
;; init modeline
;; =============================================================================

(use-package winnum :config (winum-mode))

(use-package telephone-line
  :config
  (progn
    (telephone-line-defsegment* telephone-line-datetime-segment ()
                                (format-time-string "%m-%d %k:%M"))

    (setq telephone-line-lhs
          '((winum  . (telephone-line-window-number-segment))
            (buffer . (telephone-line-buffer-segment))
            (mode   . (telephone-line-major-mode-segment))))

    (setq telephone-line-rhs
          '((info     . (telephone-line-atom-encoding-segment
                         telephone-line-atom-eol-segment))
            (time     . (telephone-line-datetime-segment))
            (position . (telephone-line-airline-position-segment))))

    (telephone-line-mode)))

(provide 'core-themes)
;;; core-themes.el ends here
