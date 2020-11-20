;;; init-org.el --- init org config -*- lexical-binding: t; -*-

;;; Code:

(use-package org
  :commands (org-clock-out org-occur-in-agenda-files org-agenda-files orgtbl-mode)
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
    ;; simple template
    (require 'org-tempo)
    ;; https://github.com/syl20bnr/spacemacs/commit/145126875731e8ee38770b2adf709805f23672f7
    ;; https://github.com/integral-dw/org-superstar-mode#hide-leading-stars
    ;; This is usually the default, but keep in mind it must be nil
    (setq org-hide-leading-stars nil)
    ;; This line is necessary.
    (setq org-superstar-leading-bullet ?\s)
    ;; https://github.com/integral-dw/org-superstar-mode#org-superstar-prettify-item-bullets
    (setq org-superstar-prettify-item-bullets nil)

    (with-eval-after-load 'org
      (progn
        ;; ===========================================================================
        ;; org-mode gtd state settings
        ;; @ /! Switch to this state will be prompted to enter
        ;; ===========================================================================

        (setq org-todo-keywords
              '((sequence "TODO(t!)" "NEXT(n)" "WAIT(w)" "|" "DONE(d@/!)" "ABORT(a@/!)")))))))

(use-package org-superstar
  :defer t
  :init (add-hook 'org-mode-hook 'org-superstar-mode))

(use-package toc-org
  :defer t
  :init
  (progn
    (setq toc-org-max-depth 10)
    (add-hook 'org-mode-hook 'toc-org-enable)))

(use-package htmlize
  :defer t)

;;; init-org.el ends here