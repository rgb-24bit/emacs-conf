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

(use-package winum :config (winum-mode))

;; (use-package telephone-line
;;   :config
;;   (progn
;;     (telephone-line-defsegment* telephone-line-datetime-segment ()
;;                                 `(,(format-time-string "%m-%d %k:%M")))

;;     (setq telephone-line-lhs
;;           '((evil     . (telephone-line-window-number-segment))
;;             (accent  . (telephone-line-buffer-modified-segment
;;                        telephone-line-filesize-segment
;;                        telephone-line-buffer-name-segment))
;;             (nil   . (telephone-line-major-mode-segment))))

;;     (setq telephone-line-rhs
;;           '((nil     . (telephone-line-atom-encoding-segment
;;                          telephone-line-atom-eol-segment))
;;             (accent      . (telephone-line-datetime-segment))
;;             (evil  . (telephone-line-airline-position-segment))))

;;     (telephone-line-mode)))


(use-package spaceline-config
  :config
  (progn
    (spaceline-spacemacs-theme)

    (spaceline-toggle-minor-modes-off)

    (spaceline-define-segment datetime
      (format-time-string "%m-%d %k:%M"))
    (spaceline-spacemacs-theme 'datetime)))



;; =============================================================================
;; init dashboard
;; =============================================================================

(use-package dashboard
  :config
  (progn
    (dashboard-setup-startup-hook)
    (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard"
          dashboard-startup-banner 'logo
          dashboard-center-content t
          dashboard-show-shortcuts nil
          dashboard-items '((recents  . 5)
                            (projects . 5)))))

;; =============================================================================
;; display line number
;; =============================================================================

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

(setq display-line-numbers-width-start t)

;; =============================================================================
;; remove gui elements
;; =============================================================================

(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode -1)

(provide 'core-themes)
;;; core-themes.el ends here
