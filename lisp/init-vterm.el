;;; init-vterm.el --- init vterm config -*- lexical-binding: t; -*-

;;; Code:

(defvar emacsc-shell-default-width 30)

(use-package vterm
  :defer t
  :commands (vterm vterm-other-window)
  :init (emacsc-leader-def "'" 'vterm-other-window)
  :config
  (progn
    (setq vterm-shell shell-file-name)

    (define-key vterm-mode-map (kbd "M-n") 'vterm-send-down)
    (define-key vterm-mode-map (kbd "M-p") 'vterm-send-up)
    (define-key vterm-mode-map (kbd "M-y") 'vterm-yank-pop)
    (define-key vterm-mode-map (kbd "M-/") 'vterm-send-tab)
    (define-key vterm-mode-map (kbd "C-y") 'vterm-yank)
    (define-key vterm-mode-map (kbd "M-m") nil)

    (defun emacsc//disable-hl-line-mode ()
      "Locally disable global-hl-line-mode"
      (interactive)
      (setq-local global-hl-line-mode nil))

    (add-hook 'vterm-mode-hook 'emacsc//disable-hl-line-mode)))

(use-package shell-pop
  :defer t
  :init (setq shell-pop-full-span t))

(defun emacsc/resize-shell-to-desired-width ()
  (when (and (string= (buffer-name) shell-pop-last-shell-buffer-name)
	     (memq shell-pop-window-position '(left right)))
    (enlarge-window-horizontally (- (/ (* (frame-width) emacsc-shell-default-width)
				       100)
				    (window-width)))))

(defun emacsc/shell-pop-vterm (index)
  "Toggle a popup window with `vterm'.
Multiple shells can be opened with a numerical prefix argument. Using the universal prefix argument will open the shell in the current buffer instead of a popup buffer."
  (interactive "P")
  (require 'shell-pop)
  (if (equal '(4) index)
      (vterm)
    (shell-pop--set-shell-type
     'shell-pop-shell-type '("vterm" "*vterm*" '(lambda () (vterm))))
    (shell-pop index)
    (emacsc/resize-shell-to-desired-width)))

(emacsc-leader-def "'" '(emacsc/shell-pop-vterm :which-key "vterm"))

(provide 'init-vterm)
;;; init-vterm.el ends here
