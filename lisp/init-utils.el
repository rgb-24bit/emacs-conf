;;; init-utils.el --- init util packages -*- lexical-binding: t; -*-

;;; Code:

;; =============================================================================
;; auto save & disable back file
;; =============================================================================

(setq make-backup-files nil)

(use-package auto-save
  :config
  (progn
    (auto-save-enable)
    (setq auto-save-silent t)
    (setq auto-save-delete-trailing-whitespace t)))

;; =============================================================================
;; ibuffer
;; =============================================================================

;; make ibuffer default
(defalias 'list-buffers 'ibuffer)

;; hide all buffers starting with an asterisk
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*")

;; =============================================================================
;; dired
;; =============================================================================

(require 'dired-x)

;; More friendly file size display
(setq-default dired-listing-switches "-alh")

;; =============================================================================
;; move-text
;; =============================================================================

(use-package move-text
  :config (move-text-default-bindings))

;; =============================================================================
;; recentf
;; =============================================================================

(use-package recentf
  :commands (recentf-save-list)
  :bind ("C-x C-r" . recentf-open-files)
  :init
  (progn
    (setq recentf-save-file (expand-file-name "recentf" emacsc-cache-directory)
	  recentf-max-saved-items 1000
	  recentf-auto-cleanup 'never
	  recentf-auto-save-timer (run-with-idle-timer 600 t
						       'recentf-save-list)))
  :config
  (progn
    (add-to-list 'recentf-exclude
		 (recentf-expand-file-name emacsc-cache-directory))
    (add-to-list 'recentf-exclude (recentf-expand-file-name package-user-dir))
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")))

;; =============================================================================
;; y-or-n
;; =============================================================================

(defalias 'yes-or-no-p 'y-or-n-p)

;; =============================================================================
;; avy
;; =============================================================================

(use-package avy
  :bind ("C-:" . avy-goto-char-in-line))

;; =============================================================================
;; code folding
;; =============================================================================

(require 'origami)

(add-hook 'prog-mode-hook 'origami-mode)

;; =============================================================================
;; view mode
;; =============================================================================

;; https://stackoverflow.com/questions/1128927/how-to-scroll-line-by-line-in-gnu-emacs/16229080#16229080
(with-eval-after-load 'view
  (progn
    (define-key view-mode-map (kbd "n") 'scroll-up-line)
    (define-key view-mode-map (kbd "p") 'scroll-down-line)))

;; https://emacs.stackexchange.com/questions/3676/how-to-enter-view-only-mode-when-browsing-emacs-source-code-from-help
(add-hook 'find-function-after-hook 'view-mode)

;; =============================================================================
;; show parent
;; =============================================================================

(add-hook 'prog-mode-hook 'show-paren-mode)

;; =============================================================================
;; undo-tree
;; =============================================================================

(require 'undo-tree)

(add-hook 'after-init-hook 'global-undo-tree-mode)

;; =============================================================================
;; clean-aindent-mode
;; =============================================================================

(use-package clean-aindent-mode
  :config
  (progn
    (clean-aindent-mode)

    (defun emacsc//put-clean-aindent-last ()
      "Put `clean-aindent--check-last-point` to end of `post-command-hook`.
This functions tries to ensure that clean-aindent checks for indent
operations after each indent operations have been done."
      (when clean-aindent-mode
	(remove-hook 'post-command-hook 'clean-aindent--check-last-point)
	(add-hook 'post-command-hook 'clean-aindent--check-last-point t)))

    (add-hook 'prog-mode-hook 'emacsc//put-clean-aindent-last t)))

;; =============================================================================
;; expan-region
;; =============================================================================

(use-package expand-region
  :bind (("C-M-." . er/expand-region)
	 ("C-M->" . er/contract-region)))

;; =============================================================================
;; highlight symbol
;; =============================================================================

(defun emacsc/regexp-at-point ()
  "if region active, return the region,
otherwise return regexp like \"\\\\_<sym\\\\_>\" for the symbol at point."
  (if (region-active-p)
      (buffer-substring-no-properties
       (region-beginning) (region-end))
    (format "\\_<%s\\_>" (thing-at-point 'symbol t))))

(defun emacsc/toggle-highlight-at-point ()
  "Toggle highlight at point (region or symbol)."
  (interactive)
  (require 'hi-lock)
  (let ((hi-regexp-list (mapcar #'car hi-lock-interactive-patterns))
	(hi-regexp-at-pt (emacsc/regexp-at-point))
	(hi-lock-auto-select-face t))
    (if (member hi-regexp-at-pt hi-regexp-list)
	(unhighlight-regexp hi-regexp-at-pt)
      (highlight-phrase hi-regexp-at-pt (hi-lock-read-face-name)))
    (deactivate-mark)))

(defun emacsc/clear-all-highlight ()
  "clear all highlight."
  (interactive)
  (let ((hi-regexp-list (mapcar #'car hi-lock-interactive-patterns)))
    (mapcar 'unhighlight-regexp hi-regexp-list)))

(global-set-key (kbd "C-'") 'emacsc/toggle-highlight-at-point)

;; =============================================================================
;; rainbow delimiters
;; =============================================================================

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init-utils)
;;; init-utils.el ends here
