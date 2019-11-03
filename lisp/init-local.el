;;; init-local.el --- Custom User Settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq mac-option-key-is-meta t)
(setq mac-command-key-is-meta nil)
(setq mac-command-modifier nil)
(setq mac-option-modifier 'meta)

(load-theme `sanityinc-tomorrow-day t)

;;; Send all auto-saves to one directory.
(setq backup-directory-alist '(("." . "~//.saves")))

(provide 'init-local)
;;; init-local.el ends here
