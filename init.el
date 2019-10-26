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

;; No Eldoc, it was causing performance problems.
(global-eldoc-mode -1)

(eval-when-compile
  (require 'use-package))

;; Key re-bindings
;; Give imenu a shortcut.
;; This nukes tab-to-tab-stop, which you'll never ever use.
(global-set-key (kbd "M-i") 'imenu)

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
 '(package-selected-packages
   (quote
    (buffer-move counsel swiper slime helm-config helm adaptive-wrap visual-fill-column gruvbox-theme lsp-mode company-lsp lsp-ui tide rjsx-mode exec-path-from-shell ## magit base16-theme web-mode)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

;; move cursor by camelCase
(global-subword-mode 1)
;; 1 for on, 0 for off

;; Don't truncate lines, make them wrap.
(setq truncate-lines nil)

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
(use-package gruvbox-theme
  :ensure t
  :config
  (load-theme 'gruvbox-light-soft t))

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

(use-package yasnippet :ensure t)

;; RJSX mode file registration.
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

;; Toggle visual-line-mode in Org-mode so lines wrap nicely.
(add-hook 'org-mode-hook 'visual-line-mode)

;; Clutch package that makes visual wraps align with indentation.
(use-package adaptive-wrap
  :ensure t
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

;; Send all the auto-save files to their own directory.
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; Helm things, disabled until mastered IDO.
;; (global-set-key (kbd "M-x") #'helm-M-x)
;; (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
;; (global-set-key (kbd "C-x C-f") #'helm-find-files)
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "C-c h x") 'helm-register)

;; (use-package helm
;;   :ensure t
;;   :bind (
;; 	 :map helm-command-map
;; 	      ("b" . helm-buffers-list)
;; 	      ("f" . helm-find-files)
;; 	      ("m" . helm-mini)
;; 	      ("o" . helm-imenu)))

;; (helm-mode 1)

;; Ivy and Counsel
(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c a") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))


;; Ido mode
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)

;; Set default folder for orgmode capture.
(setq org-default-notes-file "~/orgmode/notes.org")

;; Set default file for orgmode diary.
(setq diary-file "~/orgmode/diary")

;; Bind org-capture to easy shortcut. C-c c is recommended by manual.
(global-set-key (kbd "C-c c") 'org-capture)

;; Lisp setup
(use-package slime
  :ensure t)
(setq inferior-lisp-program "/usr/local/Cellar/sbcl/1.5.7_1/bin/sbcl")
(setq slime-contribs '(slime-fancy))

;; Magit setup
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; Org refile setup
(setq org-refile-targets '((nil :maxlevel . 9)
                                (org-agenda-files :maxlevel . 9)))

(setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
(setq org-refile-use-outline-path t)                  ; Show full paths for refiling

;; Set install buffer-move and bindings
(use-package buffer-move
  :ensure t
  :bind (("<C-S-up>" . buf-move-up)
	 ("<C-S-down>" . buf-move-down)
	 ("<C-S-left>" . buf-move-left)
	 ("<C-S-right>" . buf-move-right)))

(setq buffer-move-stay-after-swap t)
