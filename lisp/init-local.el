;;; init-local.el --- Custom User Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq mac-option-key-is-meta t)
(setq mac-command-key-is-meta nil)
(setq mac-command-modifier nil)
(setq mac-option-modifier 'meta)

;;; Load preferred theme here.
(load-theme `sanityinc-tomorrow-day t)

;;; Send all auto-saves to one directory.
(setq backup-directory-alist '(("." . "~//.saves")))

;;; In Orgmode, don't allow accidental editing of a collapsed section.
(setq org-catch-invisible-edits 'error)

(provide 'init-local)

;;; CUA mode is super annoying. Turn it off.
(cua-selection-mode nil)

;;; init-local.el ends here
