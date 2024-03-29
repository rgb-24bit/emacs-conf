;;; core-keybindings.el --- init keybinding config -*- lexical-binding: t; -*-

;;; Code:

(use-package-straight general
  :config
  ;; To automatically prevent Key sequence starts with a non-prefix key errors
  (general-auto-unbind-keys))


;; =============================================================================
;; which-key
;; =============================================================================

(use-package-straight which-key
  :config
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

  (which-key-mode))

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
  "f"     '(:ignore t :which-key "files")
  "f D"   'emacsc/delete-current-buffer-file
  "f C"   'emacsc/copy-file
  "f R"   'emacsc/rename-file-and-buffer

  "f e"   '(:ignore t :which-key "emacs")
  "f e d" 'emacsc/open-init-file
  "f e p" 'emacsc/open-environ-config
  "f e R" 'emacsc/load-init-file
  "f e c" 'emacsc/recompile-site-lisp-packages

  "f v"     '(:ignore t :which-key "variables")
  "f v d"   'add-dir-local-variable

  ;; funcs
  "TAB"   'emacsc/alternate-buffer

  "b"     '(:ignore t :which-key "buffers")
  "b d"   'emacsc/kill-this-buffer

  "j"     '(:ignore t :which-key "jump/join/split")
  "j ="   'emacsc/indent-region-or-buffer

  "T"     '(:ignore t :which-key "UI/Themes")
  "T F"   'emacsc/toggle-frame-fullscreen
  "T T"   'emacsc/adjust-frame-opacity
  "T ="   'emacsc/adjust-frame-opacity-+2
  "T +"   'emacsc/adjust-frame-opacity-+2
  "T -"   'emacsc/adjust-frame-opacity--2
  "T _"   'emacsc/adjust-frame-opacity--2

  "a"     '(:ignore t :which-key "applications")
  "a p"   'list-processes

  "x"     '(:ignore t :which-key "text")
  "x d"   '(:ignore t :which-key "delete")
  "x d w" 'delete-trailing-whitespace
  "x d l" 'delete-trailing-lines
  )

(general-def
  "M-DEL"        'cycle-spacing

  "S-SPC"        'set-mark-command

  "S-C-<left>"   'shrink-window-horizontally
  "S-C-<right>"  'enlarge-window-horizontally
  "S-C-<down>"   'shrink-window
  "S-C-<up>"     'enlarge-window

  "C-c C-n"      'emacsc/jump-next-func
  "C-c C-p"      'emacsc/jump-prev-func
  "C-c C-d"      'emacsc/duplicate-line-or-region

  "C-x <left>"   'windmove-left
  "C-x <right>"  'windmove-right
  "C-x <down>"   'windmove-down
  "C-x <up>"     'windmove-up)

;; =============================================================================
;; macos key modifier
;; =============================================================================

(when emacsc-system-is-mac
  (setq mac-command-modifier       'meta
        mac-option-modifier        'meta
        mac-function-modifier      'control
        mac-control-modifier       'control
        mac-right-command-modifier 'left
        mac-right-option-modifier  'left
        mac-right-control-modifier 'left))

(provide 'core-keybindings)
;;; core-keybindings.el ends here
