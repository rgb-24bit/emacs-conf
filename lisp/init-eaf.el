;;; init-eaf.el --- init eaf config -*- lexical-binding: t; -*-

;;; Code:

(use-package eaf
  :defer t
  :commands (eaf-open-browser
	     eaf-open-browser-with-history
	     eaf-open)
  :init
  (use-package epc :defer t)
  (use-package ctable :defer t)
  (use-package deferred :defer t)
  (use-package s :defer t)
  :custom
  (eaf-browser-continue-where-left-off t)
  :config
  (eaf-setq eaf-browser-enable-adblocker "true")
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki

(provide 'init-eaf)
;;; init-eaf.el ends here
