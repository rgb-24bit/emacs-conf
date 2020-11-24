;;; core-keybindings.el --- init keybinding config -*- lexical-binding: t; -*-

;;; Code:

(require 'general)

;; To automatically prevent Key sequence starts with a non-prefix key errors
(general-auto-unbind-keys)

;; =============================================================================
;; leader key
;; =============================================================================

(defconst emacsc-leader-key "M-m")

(general-create-definer emacsc-leader-def :prefix emacsc-leader-key)

;; =============================================================================
;; core keybindings
;; =============================================================================

(emacsc-leader-def
 ;; open & load config file
 "f e d" 'emacsc/open-init-file
 "f e p" 'emacsc/open-environ-config
 "f e R" 'emacsc/load-init-file
 ;; recompile packages
 "f e c" 'emacsc/recompile-site-lisp-packages
 )

(provide 'core-keybindings)
;;; core-keybindings.el ends here
