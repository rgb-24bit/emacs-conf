;;; init.el --- init emacs config -*- lexical-binding: t -*-

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;;(setq debug-on-error t)

;; =============================================================================
;; define base lisp dirctory
;; =============================================================================

(defconst emacsc-lisp-directory (expand-file-name "lisp" user-emacs-directory))
(defconst emacsc-core-directory (expand-file-name "core" emacsc-lisp-directory))

(add-to-list 'load-path emacsc-lisp-directory)
(add-to-list 'load-path emacsc-core-directory)

;; =============================================================================
;; define system type
;; =============================================================================

(defconst emacsc-system-is-mac     (eq system-type 'darwin))
(defconst emacsc-system-is-linux   (eq system-type 'gnu/linux))
(defconst emacsc-system-is-windows (memq system-type '(cygwin windows-nt ms-dos)))

;; =============================================================================
;; Adjust garbage collection thresholds during startup, and thereafter
;; =============================================================================

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; =============================================================================
;; Pre config <= emacs 29
;; =============================================================================

(cond
 (emacsc-system-is-windows
  (set-face-attribute
   'default nil
   :font (font-spec :name "-outline-Source Code Pro-bold-italic-normal-mono-*-*-*-*-c-*-iso10646-1"
                    :weight 'normal
                    :slant 'normal
                    :size 10.0))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     (font-spec :name "-outline-微软雅黑-normal-normal-normal-sans-*-*-*-*-p-*-iso10646-1"
                :weight 'normal
                :slant 'normal
                :size 12.0))))

 (emacsc-system-is-linux
  (set-face-attribute
   'default nil
   :font (font-spec :name "-ADBO-Source Code Pro-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1"
                    :weight 'normal
                    :slant 'normal
                    :size 12.5))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     (font-spec :name "-SINO-华文楷体-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1"
                :weight 'normal
                :slant 'normal
                :size 15.0)))))

;; =============================================================================
;; Bootstrap config
;; =============================================================================

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'core)

(require 'init-utils)
(require 'init-org)
(require 'init-yasnippet)
(require 'init-projectile)
(require 'init-ivy)
(require 'init-completion)
(require 'init-ledger)
(require 'init-md)
(require 'init-lsp)
(require 'init-rust)
(require 'init-python)
(require 'init-major-mode)
(require 'init-go)
(require 'init-vc)
(require 'init-treemacs)

(when (not emacsc-system-is-windows)
  (require 'init-vterm))

;; (when (not emacsc-system-is-mac)
;;   (require 'init-eaf))


(when emacsc-system-is-linux
  (require 'init-rime))

(when (file-exists-p custom-file)
  (load custom-file))

;; =============================================================================
;; cnfonts
;; =============================================================================

(use-package-straight cnfonts
  :config (cnfonts-mode))

;; =============================================================================
;; start server
;; =============================================================================

(require 'server)

(setq server-socket-dir (expand-file-name "server" user-emacs-directory))

(unless (server-running-p)
  (message "Starting a server...")
  (server-start))

;;; init.el ends here
