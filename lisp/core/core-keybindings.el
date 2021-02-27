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
 ;; recompile packages=
 "f e c" 'emacsc/recompgile-site-lisp-packages
 ;; funcs
 "TAB"   'emacsc/alternate-buffer
 "j ="   'emacsc/indent-region-or-buffer
 )

;; =============================================================================
;; which-key
;; =============================================================================

(require 'which-key)

(setq which-key-add-column-padding 1
      which-key-allow-multiple-replacements t
      which-key-echo-keystrokes 0.02
      which-key-idle-delay 0.4
      which-key-idle-secondary-delay 0.01
      which-key-max-description-length 32
      which-key-max-display-columns nil
      which-key-min-display-lines 2
      which-key-prevent-C-h-from-cycling t
      which-key-sort-order 'which-key-prefix-then-key-order
      which-key-sort-uppercase-first nil
      which-key-special-keys nil
      which-key-use-C-h-for-paging t
      which-key-allow-evil-operators t)

(which-key-mode)

(provide 'core-keybindings)
;;; core-keybindings.el ends here
