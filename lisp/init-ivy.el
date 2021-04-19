;;; init-ivy.el --- init ivy config -*- lexical-binding: t; -*-

;;; Code:

;; =============================================================================
;; ivy
;; =============================================================================

(use-package-straight smex)
(use-package-straight ivy)

(with-eval-after-load 'ivy
  (setq-default ivy-use-virtual-buffers t
		ivy-virtual-abbreviate 'fullpath
		ivy-count-format ""
		projectile-completion-system 'ivy
		ivy-magic-tilde nil
		ivy-dynamic-exhibit-delay-ms 150
		ivy-use-selectable-prompt t)

  ;; mappings to quit minibuffer or enter transient state
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (define-key ivy-minibuffer-map (kbd "C-<return>") #'ivy-alt-done)

  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))

(add-hook 'after-init-hook 'ivy-mode)

;; =============================================================================
;; counsel
;; =============================================================================

(require 'counsel)

(setq-default counsel-mode-override-describe-bindings t)

(with-eval-after-load 'counsel
  (setq-default ivy-initial-inputs-alist
		'((Man-completion-table . "^")
		  (woman . "^")))
  ;; I prefer the default behaviour or cycling in place, or
  ;; explicit use of browse-kill-ring
  (define-key counsel-mode-map [remap yank-pop] nil)

  ;; keybindings
  (global-set-key (kbd "C-x C-r") 'counsel-recentf)
  (global-set-key (kbd "M-y") 'counsel-yank-pop)

  (emacsc-leader-def
    "s"   '(:ignore t :which-key "search")
    "s p" 'counsel-rg

    "T l" 'counsel-load-theme))

(add-hook 'after-init-hook 'counsel-mode)

;; =============================================================================
;; swiper
;; =============================================================================

(require 'swiper)

(global-set-key "\C-s" 'swiper)

(provide 'init-ivy)
;;; init-ivy.el ends here
