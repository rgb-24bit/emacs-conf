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

(use-package powerline
  :config
  (progn
    (defun powerline-emacsc-theme ()
      "Setup the emacsc mode-line."
      (interactive)
      (setq-default mode-line-format
                    '("%e"
                      (:eval
                       (let* ((active (powerline-selected-window-active))
                              (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                              (mode-line (if active 'mode-line 'mode-line-inactive))
                              (face0 (if active 'powerline-active0 'powerline-inactive0))
                              (face1 (if active 'powerline-active1 'powerline-inactive1))
                              (face2 (if active 'powerline-active2 'powerline-inactive2))
                              (separator-left (intern (format "powerline-%s-%s"
                                                              (powerline-current-separator)
                                                              (car powerline-default-separator-dir))))
                              (separator-right (intern (format "powerline-%s-%s"
                                                               (powerline-current-separator)
                                                               (cdr powerline-default-separator-dir))))
                              (lhs (list (powerline-raw "%*" face0 'l)
                                         (when powerline-display-buffer-size
                                           (powerline-buffer-size face0 'l))
                                         (when powerline-display-mule-info
                                           (powerline-raw mode-line-mule-info face0 'l))
                                         (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                                         (when (and (boundp 'which-func-mode) which-func-mode)
                                           (powerline-raw which-func-format face0 'l))
                                         (powerline-raw " " face0)
                                         (funcall separator-left face0 face1)
                                         (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                           (powerline-raw erc-modified-channels-object face1 'l))
                                         (powerline-major-mode face1 'l)
                                         (powerline-process face1)
                                         (powerline-narrow face1 'l)
                                         (powerline-raw " " face1)
                                         (funcall separator-left face1 face2)
                                         (powerline-vc face2 'r)
                                         (when (bound-and-true-p nyan-mode)
                                           (powerline-raw (list (nyan-create)) face2 'l))))
                              (rhs (list (powerline-raw global-mode-string face2 'r)
                                         (funcall separator-right face2 face1)
                                         (unless window-system
                                           (powerline-raw (char-to-string #xe0a1) face1 'l))
                                         (powerline-raw "%4l" face1 'l)
                                         (powerline-raw ":" face1 'l)
                                         (powerline-raw "%3c" face1 'r)
                                         (funcall separator-right face1 face0)
                                         (powerline-raw " " face0)
                                         (powerline-raw "%6p" face0 'r)
                                         (when powerline-display-hud
                                           (powerline-hud face0 face2))
                                         (powerline-fill face0 0)
                                         )))
                         (concat (powerline-render lhs)
                                 (powerline-fill face2 (powerline-width rhs))
                                 (powerline-render rhs)))))))
    (powerline-emacsc-theme)))

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
