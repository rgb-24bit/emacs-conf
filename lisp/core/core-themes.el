;;; core-themes.el --- emacs theme config -*- lexical-binding: t; -*-

;;; Code:

;; =============================================================================
;; init themes
;; =============================================================================

(use-package-straight doom-themes)

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(doom-one))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)

;; =============================================================================
;; init frame
;; =============================================================================

(setq frame-title-format "rgb-24bit's emacs")

;; =============================================================================
;; init modeline
;; =============================================================================

(use-package-straight winum
  :init
  (setq winum-keymap
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
          (define-key map (kbd "M-1") 'winum-select-window-1)
          (define-key map (kbd "M-2") 'winum-select-window-2)
          (define-key map (kbd "M-3") 'winum-select-window-3)
          (define-key map (kbd "M-4") 'winum-select-window-4)
          (define-key map (kbd "M-5") 'winum-select-window-5)
          (define-key map (kbd "M-6") 'winum-select-window-6)
          (define-key map (kbd "M-7") 'winum-select-window-7)
          (define-key map (kbd "M-8") 'winum-select-window-8)
          map)
        winum-auto-setup-mode-line nil)
  :hook (after-init . winum-mode))

;; =============================================================================
;; modeline
;; =============================================================================

(use-package-straight doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq display-time-format "%m-%d %k:%M")
  (display-time)
  (setq doom-modeline-icon nil)
  ;; (doom-modeline-def-modeline 'emacsc-modeline
  ;;   '(bar window-number buffer-info major-mode matches checker vcs remote-host parrot selection-info)
  ;;   '(misc-info minor-modes input-method buffer-encoding process buffer-position))
  ;; (defun setup-custom-doom-modeline ()
  ;;  (doom-modeline-set-modeline 'emacsc-modeline 'default))
  ;; (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline)
  )

;; =============================================================================
;; init page-break-lines
;; =============================================================================

(use-package-straight page-break-lines)

;; =============================================================================
;; init dashboard
;; =============================================================================

(use-package-straight dashboard
  :config
  (progn
    (dashboard-setup-startup-hook)
    (setq dashboard-banner-logo-title "Welcome to rgb-24bit's Emacs"
          dashboard-startup-banner "~/.emacs.d/banners/bannber.png"
          dashboard-center-content t
          dashboard-show-shortcuts nil
          dashboard-items '((recents  . 5)
                            (projects . 5)))))

;; =============================================================================
;; display line & column number
;; =============================================================================

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

(setq display-line-numbers-width-start t)

(add-hook 'after-init-hook 'column-number-mode)

;; =============================================================================
;; hilight cursor line
;; =============================================================================

(require 'hl-line)

(add-hook 'after-init-hook 'global-hl-line-mode)

;; =============================================================================
;; display trailing whitespace
;; =============================================================================

(add-hook 'prog-mode-hook
          (lambda ()
            (set-face-attribute 'trailing-whitespace nil
                                :background
                                (face-attribute 'font-lock-comment-face
                                                :foreground))
            (setq show-trailing-whitespace t)))


;; =============================================================================
;; all the icons
;; =============================================================================

(use-package-straight all-the-icons
  :if (display-graphic-p))

;; =============================================================================
;; remove gui elements
;; =============================================================================

(scroll-bar-mode -1)
(tool-bar-mode -1)
(when (not emacsc-system-is-mac)
  (menu-bar-mode -1))
(tooltip-mode -1)

(provide 'core-themes)
;;; core-themes.el ends here
