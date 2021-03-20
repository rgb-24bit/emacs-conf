;;; core-themes.el --- emacs theme config -*- lexical-binding: t; -*-

;;; Code:

(defvar emacsc-powerline-scale (if emacsc/system-is-windows 0.5 1.0)
  "allows to quickly tweak the mode-line size to make separators look not too crappy.")

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

;; (use-package winum
;;   :init
;;   (setq winum-keymap
;; 	(let ((map (make-sparse-keymap)))
;; 	  (define-key map (kbd "C-`") 'winum-select-window-by-number)
;; 	  (define-key map (kbd "C-²") 'winum-select-window-by-number)
;; 	  (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
;; 	  (define-key map (kbd "M-1") 'winum-select-window-1)
;; 	  (define-key map (kbd "M-2") 'winum-select-window-2)
;; 	  (define-key map (kbd "M-3") 'winum-select-window-3)
;; 	  (define-key map (kbd "M-4") 'winum-select-window-4)
;; 	  (define-key map (kbd "M-5") 'winum-select-window-5)
;; 	  (define-key map (kbd "M-6") 'winum-select-window-6)
;; 	  (define-key map (kbd "M-7") 'winum-select-window-7)
;; 	  (define-key map (kbd "M-8") 'winum-select-window-8)
;; 	  map))
;;   :hook (after-init . winum-mode))

(use-package window-numbering
  :hook (after-init . window-numbering-mode))

(defun emacsc/compute-mode-line-height (scale)
  "Return an adjusted mode-line height."
  (truncate (* scale (frame-char-height))))

(use-package powerline
  :defer t
  :init
  (setq-default powerline-height (emacsc/compute-mode-line-height emacsc-powerline-scale)))

(use-package maple-modeline
  :hook (after-init . maple-modeline-mode)
  :config
  ;; standard, minimal, sidebar
  (setq maple-modeline-style 'standard)
  ;; show icon, just for version-control
  (setq maple-modeline-icon nil)
  ;; standard or auto or some number
  (setq maple-modeline-width 'standard))

;; =============================================================================
;; display time
;; =============================================================================

;; (display-time-mode -1)

;; (setq display-time-24hr-format t)

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
;; hilight cursor line
;; =============================================================================

(require 'hl-line)

(add-hook 'after-init-hook 'global-hl-line-mode)

;; =============================================================================
;; remove gui elements
;; =============================================================================

(scroll-bar-mode -1)
(tool-bar-mode -1)
(when (not emacsc/system-is-mac)
  (menu-bar-mode -1))
(tooltip-mode -1)

(provide 'core-themes)
;;; core-themes.el ends here
