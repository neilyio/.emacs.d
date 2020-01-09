;;; init-local.el --- Custom User Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (when (eq system-type 'darwin)
;;   (setq mac-option-key-is-meta t)
;;   (setq mac-command-key-is-meta nil)
;;   (setq mac-command-modifier nil)
;;   (setq mac-option-modifier 'meta))

;;; On Windows, C-z minimizes the widow thanks to init-gui-frames.el.
;;; Stop this madness.
(global-set-key (kbd "C-z") nil)

;;; Load preferred theme here.
(load-theme `sanityinc-tomorrow-night t)

;;; Send all auto-saves to one directory.
(setq backup-directory-alist '(("." . "~//.saves")))

;;; In Orgmode, don't allow accidental editing of a collapsed section.
(setq org-catch-invisible-edits 'error)

;;; Visual-line-mode should be on any time orgmode is on so lines wrap nicely.
(add-hook 'org-mode-hook 'visual-line-mode)

;;; Turn off paredit in orgmode. The behavior was annoying.
(add-hook 'org-mode-hook (lambda () (paredit-mode -1)))

;;; This mode lines up the line wrap with the proper indentation.
;;; Note that you have to package-install adaptive-wrap for this hook to work.
(add-hook 'visual-line-mode 'adaptive-wrap-prefix-mode)

;;; CUA mode is super annoying. Turn it off.
(cua-selection-mode -1)
(cua-mode -1)

;; Fountain mode /Olivetti for screenwriting
(require 'fountain-mode)
(require 'olivetti)
(add-hook 'fountain-mode-hook 'turn-on-olivetti-mode)

;;; Create a binding for pop-to-mark-command, which Ivy forwards to counsel-mark-ring.
(global-set-key (kbd "M-g SPC") #'pop-to-mark-command)

;; LSP-mode things.
(require 'lsp-mode)
(add-hook 'python-mode-hook #'lsp #'flycheck-mode)
(add-hook 'js-mode-hook #'lsp  #'flycheck-mode)
(add-hook 'js-jsx-mode-hook #'lsp  #'flycheck-mode)

(require 'company-lsp)
(push 'company-lsp company-backends)
(setq lsp-prefer-flymake nil)

(require 'lsp-ui)

;; Plugin for Styled Components.
(setq lsp-clients-typescript-log-verbosity "debug"
      lsp-clients-typescript-plugins
      (vector
       (list :name "typescript-styled-plugin")))

;;; Remove trailing whitespace on every save.
;;; Found at https://www.emacswiki.org/emacs/DeletingWhitespacen
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'init-local)
;;; init-local.el ends here
