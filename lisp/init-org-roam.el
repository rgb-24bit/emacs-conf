;;; init-org-roam.el --- init org roam config -*- lexical-binding: t; -*-

;;; Code:

(use-package-straight org-roam
  :custom (org-roam-directory "~/repositories/notebook")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-db-location (expand-file-name "org-roam.db" emacsc-cache-directory))

  (org-roam-setup))


;; (use-package-straight org-roam-server
;;   :commands (org-roam-server-mode)
;;   :config
;;   (setq org-roam-server-port 10204))

(provide 'init-org-roam)
;;; init-org-roam.el ends here
