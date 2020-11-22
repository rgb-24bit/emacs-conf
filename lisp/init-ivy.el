;;; init-ivy.el --- init ivy config -*- lexical-binding: t; -*-

;;; Code:

;; =============================================================================
;; ivy
;; =============================================================================

(require 'ivy)

(with-eval-after-load 'ivy
  (setq-default ivy-use-virtual-buffers t
                ivy-virtual-abbreviate 'fullpath
                ivy-count-format ""
                projectile-completion-system 'ivy
                ivy-magic-tilde nil
                ivy-dynamic-exhibit-delay-ms 150
                ivy-use-selectable-prompt t)

  ;; IDO-style directory navigation
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (dolist (k '("C-j" "C-RET"))
    (define-key ivy-minibuffer-map (kbd k) #'ivy-immediate-done))

  (define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line-or-history)
  (define-key ivy-minibuffer-map (kbd "<down>") #'ivy-next-line-or-history)

  (define-key ivy-occur-mode-map (kbd "C-c C-q") #'ivy-wgrep-change-to-wgrep-mode))

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
  (define-key counsel-mode-map [remap yank-pop] nil))

(add-hook 'after-init-hook 'counsel-mode)

;; =============================================================================
;; swiper
;; =============================================================================

(require 'swiper)

(provide 'init-ivy)
;;; init-ivy.el ends here