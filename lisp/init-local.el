;;; init-local.el --- Custom User Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; (when (eq system-type 'darwin)
;;   (setq mac-option-key-is-meta t)
;;   (setq mac-command-key-is-meta nil)
;;   (setq mac-command-modifier nil)
;;   (setq mac-option-modifier 'meta))

;;; Load preferred theme here.
(load-theme `sanityinc-tomorrow-day t)

;;; Send all auto-saves to one directory.
(setq backup-directory-alist '(("." . "~//.saves")))

;;; In Orgmode, don't allow accidental editing of a collapsed section.
(setq org-catch-invisible-edits 'error)

;;; Visual-line-mode should be on any time orgmode is on so lines wrap nicely.
(add-hook 'org-mode-hook 'visual-line-mode)

;;; This mode lines up the line wrap with the proper indentation.
;;; Note that you have to package-install adaptive-wrap for this hook to work.
(add-hook 'visual-line-mode 'adaptive-wrap-prefix-mode)

;;; CUA mode is super annoying. Turn it off.
(cua-selection-mode nil)

(provide 'init-local)
;;; init-local.el ends here
