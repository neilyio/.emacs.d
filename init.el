;; Turn on/f debug mode right away.
(setq debug-on-error nil)

;; Get USE-PACKAGE going right away.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


;; Emacs full screen on startup.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "8543b328ed10bc7c16a8a35c523699befac0de00753824d7e90148bca583f986" default)))
 '(electric-indent-mode nil)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

;; move cursor by camelCase
(global-subword-mode 1)
;; 1 for on, 0 for off

;; No welcome screen.
(setq inhibit-startup-screen t)


;; Show matching parenthesis.
(show-paren-mode 1)

;; MELPA setup here.
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Load Theme
(use-package base16-theme
  :ensure t
  :config
  (load-theme 'base16-monokai t))

;; No scroll bar #HARDCORE MODE
(setq scroll-bar-mode nil)
;; No tool bar #HARDCORE MODE
(setq tool-bar-mode nil)
;; As suggested by magit, bind this key because you'll use it constantly.
(global-set-key (kbd "C-x g") 'magit-status)
;; No audible sound on Emacs errors. Just flash the mode bar instead.
(setq visible-bell nil
      ring-bell-function 'flash-mode-line)
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))(setq visible-bell 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Make emacs use the regular terminal PATH on startup.
(when (memq window-system '(mac ns))
  (setenv "SHELL" "/bin/zsh")
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

;; INIDUM setup
;;(use-package indium
;;  :hook (js-mode indium-interaction-mode))

;; LSP-mode setup.
;; Add hooks for each language.
(use-package lsp-mode
  :hook (prog-mode . lsp-deferred)
  :commands lsp lsp-deferred)

(setq lsp-prefer-flymake nil)

(use-package lsp-ui :commands lsp-ui-mode)

(use-package company-lsp :commands company-lsp)

;; RJSX mode file registration.
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

 
