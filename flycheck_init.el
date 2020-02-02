;;; init.el --- -*- lexical-binding: t -*-
;;
;; Filename: init.el
;; Description: Initialize M-EMACS
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 10:15:28 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Tue Jan 28 16:38:48 2020 (-0800)
;;           By: Mingde (Matthew) Zeng
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d init
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is the init.el file for M-EMACS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; CheckVer
(cond ((version< emacs-version "26.1")
       (warn "M-EMACS requires Emacs 26.1 and above!"))
      ((let* ((early-init-f (expand-file-name "early-init.el" user-emacs-directory))
              (early-init-do-not-edit-d (expand-file-name "early-init-do-not-edit/" user-emacs-directory))
              (early-init-do-not-edit-f (expand-file-name "early-init.el" early-init-do-not-edit-d)))
         (and (version< emacs-version "27")
              (or (not (file-exists-p early-init-do-not-edit-f))
                  (file-newer-than-file-p early-init-f early-init-do-not-edit-f)))
         (make-directory early-init-do-not-edit-d t)
         (copy-file early-init-f early-init-do-not-edit-f t t t t)
         (add-to-list 'load-path early-init-do-not-edit-d)
         (require 'early-init))))
;; -CheckVer

;; BetterGC
(defvar better-gc-cons-threshold 67108864 ; 64mb
  "The default value to use for `gc-cons-threshold'.

If you experience freezing, decrease this.  If you experience stuttering, increase this.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))
;; -BetterGC

;; AutoGC
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
                              (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
              (add-hook 'after-focus-change-function 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))
;; -AutoGC

;; LoadPath
(defun update-to-load-path (folder)
  "Update FOLDER and its subdirectories to `load-path'."
  (let ((base folder))
    (unless (member base load-path)
      (add-to-list 'load-path base))
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (unless (member base load-path)
            (add-to-list 'load-path name)))))))

(update-to-load-path (expand-file-name "elisp" user-emacs-directory))
;; -LoadPath

;; Constants

(require 'init-const)

;; Packages

;; Package Management
(require 'init-package)

;; Global Functionalities
(require 'init-global-config)

(require 'init-func)

(require 'init-search)

(require 'init-crux)

(require 'init-avy)

(require 'init-winner)

(require 'init-which-key)

(require 'init-popup-kill-ring)

(require 'init-undo-tree)

(require 'init-discover-my-major)

(require 'init-ace-window)

(require 'init-shell)

(require 'init-dired)

;; User Interface Enhancements
(require 'init-ui-config)

(require 'init-all-the-icons)

(require 'init-theme)

(require 'init-dashboard)

(require 'init-fonts)

(require 'init-scroll)

;; General Programming
(require 'init-magit)

(require 'init-projectile)

(require 'init-treemacs)

(require 'init-yasnippet)

(require 'init-flycheck)

(require 'init-dumb-jump)

(require 'init-parens)

(require 'init-indent)

(require 'init-quickrun)

(require 'init-format)

(require 'init-comment)

(require 'init-edit)

(require 'init-header)

(require 'init-ein)

(require 'init-lsp)

(require 'init-company)

;; Programming

(require 'init-java)

(require 'init-cc)

(require 'init-python)

(require 'init-haskell)

(require 'init-latex)

(require 'init-ess)

;; Web Development
(require 'init-webdev)

;; Miscellaneous
(require 'init-org)

(require 'init-eaf)

(require 'init-erc)

(require 'init-eww)

(require 'init-mu4e)

(require 'init-tramp)

(require 'init-pdf)

(require 'init-leetcode)

(require 'init-pyim)

(require 'init-epaint)

(require 'init-games)

(require 'init-zone)

(provide 'init)

;;; Neil edits here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; On Windows, C-z minimizes the widow thanks to init-gui-frames.el.
;;; Stop this madness.
(global-set-key (kbd "C-z") nil)

;;; Use command as meta on mac.
(setq mac-command-modifier 'meta)

;;; Org-mode preferences
(with-eval-after-load 'org
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1))))

(setq org-catch-invisible-edits 'error)

;;; Bring back the leading stars while using org-indent-mode.
;;; org-indent mode adds buffer local variables for org-hide-leading-stars.
;;; It does this based off the value of org-indent-mode-turns-on-hiding-stars.
;;; Also using setq-default here for reassurance. Trying to nuke org-hide-leading-stars.
(setq-default org-hide-leading-stars nil)
(setq org-indent-mode-turns-on-hiding-stars nil)

;;; Reclaim the delete-word function that's nuked by delete-block.
(global-set-key (kbd "M-d") 'kill-word)
(global-set-key (kbd "M-<backspace>") 'backward-kill-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Neil edits ends here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
