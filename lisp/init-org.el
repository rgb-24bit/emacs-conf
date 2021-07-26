;;; init-org.el --- init org config -*- lexical-binding: t; -*-

;;; Code:

(defcustom emacsc-org-capture-task-file "~/task.org"
  "org capture task file location.")
(defcustom emacsc-org-capture-idea-file "~/idea.org"
  "org capture idea file location.")

(use-package org
  :commands (org-clock-out org-occur-in-agenda-files org-agenda-files orgtbl-mode)
  :straight org-plus-contrib
  :defer t
  :config
  (progn
    ;; org-mode programming language support settings
    (setq org-babel-load-languages
          '((C          . t)
            (sql        . t)
            (java       . t)
            (ledger     . t)
            (latex      . t)
            (sqlite     . t)
            (python     . t)
            (plantuml   . t)
            (emacs-lisp . t)
            (octave     . t)
            (dot        . t)
            (ditaa      . t)))
    ;; Set the hidden font style mark, the direct effect
    (setq org-hide-emphasis-markers t)
    ;; Set to prevent editing of invisible text
    (setq org-catch-invisible-edits 'error)
    ;; The setting code is highlighted
    (setq org-src-fontify-natively t)
    ;; startup with inline image
    (setq org-startup-with-inline-images t)
    ;; not change headline face
    (setq org-fontify-done-headline nil)
    ;; simple template
    (require 'org-tempo)
    ;; export
    (require 'ox-gfm)
    ;; https://github.com/syl20bnr/spacemacs/commit/145126875731e8ee38770b2adf709805f23672f7
    ;; https://github.com/integral-dw/org-superstar-mode#hide-leading-stars
    ;; This is usually the default, but keep in mind it must be nil
    (setq org-hide-leading-stars nil)
    ;; This line is necessary.
    (setq org-superstar-leading-bullet ?\s)
    ;; https://github.com/integral-dw/org-superstar-mode#org-superstar-prettify-item-bullets
    (setq org-superstar-prettify-item-bullets nil)

    (defun emacsc//org-delete-element ()
      "Delete org element."
      (interactive)
      (org-mark-element)
      (kill-region (region-beginning) (region-end)))
    (define-key org-mode-map (kbd "C-c e d") 'emacsc//org-delete-element)

    (with-eval-after-load 'org
      (progn
        ;; ===========================================================================
        ;; org-mode gtd state settings
        ;; @ /! Switch to this state will be prompted to enter
        ;; ===========================================================================

        (setq org-todo-keywords
              '((sequence "TODO(t!)" "NEXT(n)" "WAIT(w)" "|" "DONE(d@/!)" "ABORT(a@/!)")))))))

(use-package org-agenda
  :defer t
  :commands (org-agenda)
  :config
  (define-key org-agenda-mode-map (kbd "M-m") nil))

(use-package org-capture
  :bind (("C-c c" . org-capture))
  :config
  ;; Org-capture template settings
  ;; https://www.zmonster.me/2018/02/28/org-mode-capture.html
   (setq org-capture-templates
        '(("t" "Task" entry (file+headline emacsc-org-capture-task-file "Task")
           "* TODO [#B] %^{HEADLINE} %^g\n  %?"
           :empty-lines 1)
          ("i" "Idea")
          ("io" "Idea.O" entry (file emacsc-org-capture-idea-file)
           "* Idea.O %^{HEADLINE}\n  %?"
           :empty-lines 1)
          ("is" "Idea.S" entry (file emacsc-org-capture-idea-file)
           "* Idea.S %^{HEADLINE}\n  %?"
           :empty-lines 1))))

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

(use-package org-superstar
  :commands (org-superstar-mode)
  :init (add-hook 'org-mode-hook 'org-superstar-mode))

(use-package toc-org
  :commands (toc-org-enable)
  :init
  (progn
    (setq toc-org-max-depth 10)
    (add-hook 'org-mode-hook 'toc-org-enable)))

(use-package-straight htmlize
  :defer t)

(provide 'init-org)
;;; init-org.el ends here
