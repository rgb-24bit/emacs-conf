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
    (setq auto-save-silent t)))

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
;; Make dired "guess" target directory for some operations, like copy to
;; directory visited in other split buffer.
(setq dired-dwim-target t)


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

(use-package-straight avy
  :bind ("C-:" . avy-goto-char))

;; =============================================================================
;; code folding
;; =============================================================================

(use-package-straight origami
  :config
  (general-def origami-mode-map
    "C-=" 'origami-open-node
    "C--" 'origami-close-node))

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

(setq undo-tree-visualizer-timestamps t
      undo-tree-visualizer-diff t
      undo-limit 800000
      undo-strong-limit 12000000
      undo-outer-limit 120000000)

(add-hook 'after-init-hook 'global-undo-tree-mode)

;; =============================================================================
;; clean-aindent-mode
;; =============================================================================

(use-package-straight clean-aindent-mode
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

(use-package-straight expand-region
  :bind (("C-M-." . er/expand-region)
         ("C-M->" . er/contract-region)))

;; =============================================================================
;; symbol overlay
;; =============================================================================

(use-package-straight symbol-overlay
  :commands symbol-overlay-put
  :bind ("C-'" . symbol-overlay-put))

;; (use-package-straight highlight-symbol
;;   :commands highlight-symbol
;;   :bind ("C-'" . highlight-symbol))

;; (defun emacsc/regexp-at-point ()
;;   "if region active, return the region,
;; otherwise return regexp like \"\\\\_<sym\\\\_>\" for the symbol at point."
;;   (if (region-active-p)
;;       (buffer-substring-no-properties
;;        (region-beginning) (region-end))
;;     (format "\\_<%s\\_>" (thing-at-point 'symbol t))))

;; (defun emacsc/toggle-highlight-at-point ()
;;   "Toggle highlight at point (region or symbol)."
;;   (interactive)
;;   (require 'hi-lock)
;;   (let ((hi-regexp-list (mapcar #'car hi-lock-interactive-patterns))
;;         (hi-regexp-at-pt (emacsc/regexp-at-point))
;;         (hi-lock-auto-select-face t))
;;     (if (member hi-regexp-at-pt hi-regexp-list)
;;         (unhighlight-regexp hi-regexp-at-pt)
;;       (highlight-phrase hi-regexp-at-pt (hi-lock-read-face-name)))
;;     (deactivate-mark)))

;; (defun emacsc/clear-all-highlight ()
;;   "clear all highlight."
;;   (interactive)
;;   (let ((hi-regexp-list (mapcar #'car hi-lock-interactive-patterns)))
;;     (mapcar 'unhighlight-regexp hi-regexp-list)))

;; (global-set-key (kbd "C-'") 'emacsc/toggle-highlight-at-point)

;; =============================================================================
;; rainbow delimiters
;; =============================================================================

(use-package-straight rainbow-delimiters
  :commands rainbow-delimiters-mode
  :hook (prog-mode . rainbow-delimiters-mode))

;; =============================================================================
;; awesome pair
;; =============================================================================

(use-package-straight awesome-pair
  :commands awesome-pair-mode
  :init
  (dolist (hook (list
                 'c-mode-common-hook
                 'c-mode-hook
                 'c++-mode-hook
                 'java-mode-hook
                 'haskell-mode-hook
                 'emacs-lisp-mode-hook
                 'lisp-interaction-mode-hook
                 'lisp-mode-hook
                 'maxima-mode-hook
                 'ielm-mode-hook
                 'sh-mode-hook
                 'makefile-gmake-mode-hook
                 'php-mode-hook
                 'python-mode-hook
                 'js-mode-hook
                 'go-mode-hook
                 'qml-mode-hook
                 'jade-mode-hook
                 'css-mode-hook
                 'ruby-mode-hook
                 'coffee-mode-hook
                 'rust-mode-hook
                 'qmake-mode-hook
                 'lua-mode-hook
                 'swift-mode-hook
                 'minibuffer-inactive-mode-hook
                 ))
    (add-hook hook #'(lambda () (awesome-pair-mode))))
  :config
  (define-key awesome-pair-mode-map (kbd "(") 'awesome-pair-open-round)
  (define-key awesome-pair-mode-map (kbd "[") 'awesome-pair-open-bracket)
  (define-key awesome-pair-mode-map (kbd "{") 'awesome-pair-open-curly)
  (define-key awesome-pair-mode-map (kbd ")") 'awesome-pair-close-round)
  (define-key awesome-pair-mode-map (kbd "]") 'awesome-pair-close-bracket)
  (define-key awesome-pair-mode-map (kbd "}") 'awesome-pair-close-curly)
  (define-key awesome-pair-mode-map (kbd "=") 'awesome-pair-equal)

  (define-key awesome-pair-mode-map (kbd "%") 'awesome-pair-match-paren)
  (define-key awesome-pair-mode-map (kbd "\"") 'awesome-pair-double-quote)

  (define-key awesome-pair-mode-map (kbd "SPC") 'awesome-pair-space)

  (define-key awesome-pair-mode-map (kbd "C-d") 'awesome-pair-forward-delete)
  (define-key awesome-pair-mode-map (kbd "C-k") 'awesome-pair-kill)

  (define-key awesome-pair-mode-map (kbd "M-\"") 'awesome-pair-wrap-double-quote)
  (define-key awesome-pair-mode-map (kbd "M-[") 'awesome-pair-wrap-bracket)
  (define-key awesome-pair-mode-map (kbd "M-{") 'awesome-pair-wrap-curly)
  (define-key awesome-pair-mode-map (kbd "M-(") 'awesome-pair-wrap-round)
  (define-key awesome-pair-mode-map (kbd "M-)") 'awesome-pair-unwrap)

  (define-key awesome-pair-mode-map (kbd "M-n") 'awesome-pair-jump-right)
  (define-key awesome-pair-mode-map (kbd "M-p") 'awesome-pair-jump-left)
  (define-key awesome-pair-mode-map (kbd "M-:") 'awesome-pair-jump-out-pair-and-newline))

;; =============================================================================
;; mwim
;; =============================================================================

(use-package mwim
  :commands (mwim-beginning-of-code-or-line mwim-end-of-line-or-code)
  :init
  (global-set-key (kbd "C-a") 'mwim-beginning-of-code-or-line)
  (global-set-key (kbd "C-e") 'mwim-end-of-line-or-code))

;; =============================================================================
;; cnfonts
;; =============================================================================

(use-package cnfonts
  :commands (cnfonts-edit-profile cnfonts-increase-fontsize cnfonts-decrease-fontsize)
  :config
  (setq cnfonts-directory (expand-file-name "cnfonts" emacsc-cache-directory)))

;; =============================================================================
;; restclient
;; =============================================================================

(use-package-straight restclient
  :mode ("\\.http\\'" . restclient-mode))

;; =============================================================================
;; one-key
;; =============================================================================

(use-package-straight one-key)

;; =============================================================================
;; thing-edit
;; =============================================================================

(use-package thing-edit
  :commands (thing-copy-word
             thing-copy-email
             thing-copy-url
             thing-copy-line
             thing-copy-comment
             thing-copy-filename

             thing-cut-word
             thing-cut-email
             thing-cut-url
             thing-cut-line
             thing-cut-comment
             thing-cut-filename)
  :init
  (one-key-create-menu
   "THING-EDIT"
   '(
     ;; Copy.
     (("w" . "Copy Word") . thing-copy-word)
     (("s" . "Copy Symbol") . thing-copy-symbol)
     (("m" . "Copy Email") . thing-copy-email)
     (("f" . "Copy Filename") . thing-copy-filename)
     (("u" . "Copy URL") . thing-copy-url)
     (("x" . "Copy Sexp") . thing-copy-sexp)
     (("g" . "Copy Page") . thing-copy-page)
     (("t" . "Copy Sentence") . thing-copy-sentence)
     (("o" . "Copy Whitespace") . thing-copy-whitespace)
     (("i" . "Copy List") . thing-copy-list)
     (("c" . "Copy Comment") . thing-copy-comment)
     (("h" . "Copy Function") . thing-copy-defun)
     (("p" . "Copy Parentheses") . thing-copy-parentheses)
     (("l" . "Copy Line") . thing-copy-line)
     (("a" . "Copy To Line Begin") . thing-copy-to-line-beginning)
     (("e" . "Copy To Line End") . thing-copy-to-line-end)
     ;; Cut.
     (("W" . "Cut Word") . thing-cut-word)
     (("S" . "Cut Symbol") . thing-cut-symbol)
     (("M" . "Cut Email") . thing-cut-email)
     (("F" . "Cut Filename") . thing-cut-filename)
     (("U" . "Cut URL") . thing-cut-url)
     (("X" . "Cut Sexp") . thing-cut-sexp)
     (("G" . "Cut Page") . thing-cut-page)
     (("T" . "Cut Sentence") . thing-cut-sentence)
     (("O" . "Cut Whitespace") . thing-cut-whitespace)
     (("I" . "Cut List") . thing-cut-list)
     (("C" . "Cut Comment") . thing-cut-comment)
     (("H" . "Cut Function") . thing-cut-defun)
     (("P" . "Cut Parentheses") . thing-cut-parentheses)
     (("L" . "Cut Line") . thing-cut-line)
     (("A" . "Cut To Line Begin") . thing-cut-to-line-beginning)
     (("E" . "Cut To Line End") . thing-cut-to-line-end)
     )
   t)
  (emacsc-leader-def "t" '(one-key-menu-thing-edit :which-key "thing edit")))

;; =============================================================================
;; compilation
;; =============================================================================

(with-eval-after-load 'compile
  (add-hook 'compilation-finish-functions 'switch-to-buffer-other-window 'compilation))

;; =============================================================================
;; separedit
;; =============================================================================

(use-package-straight separedit
  :config
  (define-key prog-mode-map (kbd "C-c '") #'separedit))

;; =============================================================================
;; pangu-spacing
;; =============================================================================

(use-package-straight pangu-spacing
  :commands pangu-spacing-space-current-buffer
  :init
  (emacsc-leader-def "x =" '(pangu-spacing-space-current-buffer :which-key "pangu spacing buffer")))

;; =============================================================================
;; multi cursor
;; =============================================================================

(use-package-straight multiple-cursors
  :commands mc/edit-lines
  :init (setq mc/always-run-for-all t))

;; =============================================================================
;; highlight-parentheses
;; =============================================================================

(use-package highlight-parentheses
  :commands highlight-parentheses-mode
  :hook (emacs-lisp-mode . highlight-parentheses-mode))

;; =============================================================================
;; open newline
;; =============================================================================

(use-package open-newline
  :bind (("M-o" . open-neline-below)
         ("M-O" . open-newline-above)))

;; =============================================================================
;; delete block
;; =============================================================================

(use-package delete-block
  :commands (delete-block-forward delete-block-backward))

;; =============================================================================
;; duplicate line
;; =============================================================================

(use-package duplicate-line
  :commands (duplicate-line-or-region-below)
  :init
  (emacsc-leader-def
    "e"    '(:ignore t :which-key "edit")
    "e d"  'duplicate-line-or-region-below))

;; =============================================================================
;; scroll
;; =============================================================================

(defun emacsc/scroll-up-one-line()
  "Scroll up one line."
  (interactive)
  (scroll-up 1))

(defun emacsc/scroll-down-one-line()
  "Scroll down one line."
  (interactive)
  (scroll-down 1))

(general-def
  "S-<up>" 'emacsc/scroll-up-one-line
  "S-<down>" 'emacsc/scroll-down-one-line)

;; =============================================================================
;; visual-regexp
;; =============================================================================

(use-package-straight visual-regexp
  :bind ("M-%" . vr/query-replace))

;; =============================================================================
;; goto-line-preview
;; =============================================================================

(use-package-straight goto-line-preview
  :init
  (global-set-key [remap goto-line] 'goto-line-preview))

;; =============================================================================
;; toggle-one-window
;; =============================================================================

(use-package toggle-one-window
  :commands toggle-one-window)

;; =============================================================================
;; color-rg
;; =============================================================================

(use-package color-rg
  :commands (color-rg-search-input	color-rg-search-input-in-project)
  :init
  (emacsc-leader-def
    "s d" 'color-rg-search-input
    "s P" 'color-rg-search-input-in-project))

(provide 'init-utils)
;;; init-utils.el ends here
