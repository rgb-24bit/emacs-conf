;;; init-org.el --- init org config -*- lexical-binding: t; -*-

;;; Code:

(defcustom emacsc-org-capture-task-file "~/task.org"
  "org capture task file location.")
(defcustom emacsc-org-capture-idea-file "~/idea.org"
  "org capture idea file location.")

(use-package org
  :commands (org-clock-out org-occur-in-agenda-files org-agenda-files orgtbl-mode)
  :straight org-contrib
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
    ;; indet as old org version
    (setq org-adapt-indentation t)
    ;; start up show 1 level
    (setq org-startup-folded 'show1level)
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
    (define-key org-mode-map (kbd "C-c SPC") 'org-table-blank-field)

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

(use-package-straight org-roam
  :custom (org-roam-directory "~/repositories/roam")
  :bind (("C-c n l"   . org-roam-buffer-toggle)
         ("C-c n f"   . org-roam-node-find)
         ("C-c n i"   . org-roam-node-insert)
         ("C-c n g"   . org-roam-graph)
         ("C-c n c"   . org-roam-capture)
         ("C-c n a"   . org-roam-alias-add)
         ("C-c n r"   . org-roam-ref-add)
         ("C-c n k a" . org-roam-alias-remove)
         ("C-c n k r" . org-roam-ref-remove))
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-db-location (expand-file-name "org-roam.db" emacsc-cache-directory)
        org-id-locations-file (expand-file-name ".org-id-locations" emacsc-cache-directory))
  :config
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Custom slug format, ref https://github.com/org-roam/org-roam/pull/1544"
    (let ((title (org-roam-node-title node))
          (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                             768 ; U+0300 COMBINING GRAVE ACCENT
                             769 ; U+0301 COMBINING ACUTE ACCENT
                             770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                             771 ; U+0303 COMBINING TILDE
                             772 ; U+0304 COMBINING MACRON
                             774 ; U+0306 COMBINING BREVE
                             775 ; U+0307 COMBINING DOT ABOVE
                             776 ; U+0308 COMBINING DIAERESIS
                             777 ; U+0309 COMBINING HOOK ABOVE
                             778 ; U+030A COMBINING RING ABOVE
                             780 ; U+030C COMBINING CARON
                             795 ; U+031B COMBINING HORN
                             803 ; U+0323 COMBINING DOT BELOW
                             804 ; U+0324 COMBINING DIAERESIS BELOW
                             805 ; U+0325 COMBINING RING BELOW
                             807 ; U+0327 COMBINING CEDILLA
                             813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                             814 ; U+032E COMBINING BREVE BELOW
                             816 ; U+0330 COMBINING TILDE BELOW
                             817 ; U+0331 COMBINING MACRON BELOW
                             )))
      (cl-flet* ((nonspacing-mark-p (char)
                                    (memq char slug-trim-chars))
                 (strip-nonspacing-marks (s)
                                         (ucs-normalize-NFC-string
                                          (apply #'string (seq-remove #'nonspacing-mark-p
                                                                      (ucs-normalize-NFD-string s)))))
                 (cl-replace (title pair)
                             (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-")  ;; convert anything not alphanumeric
                        ("__*" . "_")  ;; remove sequential underscores
                        ("^_" . "")    ;; remove starting underscore
                        ("_$" . "")))  ;; remove ending underscore
               (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
          (downcase slug)))))

  (setq org-roam-graph-link-hidden-types '("file" "custom-id" "http" "https" "fuzzy"))

  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "${slug}.org" "#+TITLE: ${title}\n")
           :empty-lines 1
           :unnarrowed  t)))

  (org-roam-setup))

(use-package-straight org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme nil
        org-roam-ui-follow nil
        org-roam-ui-update-on-save nil
        org-roam-ui-open-on-start nil
        org-roam-ui-port 35901))

(provide 'init-org)
;;; init-org.el ends here
