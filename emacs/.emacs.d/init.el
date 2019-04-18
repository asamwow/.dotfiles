;;;; init.el

;;; package management
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'no-error 'no-message)

;;; packages
(use-package graphene
  :init
  (setq graphene-default-font "SauceCodePro Nerd Font Mono-12")
  (setq graphene-variable-pitch-font "xos4 Terminus-12")
  (setq graphene-fixed-pitch-font "xos4 Terminus-12"))
(use-package format-all)
(use-package speed-type)
(use-package yasnippet
  :config
  (yas-global-mode))
(use-package yasnippet-snippets)
(use-package notmuch)
(use-package color-theme-sanityinc-tomorrow
  :config
  (color-theme-sanityinc-tomorrow--define-theme night))
(use-package org-journal
  :config
  (setq org-directory "/home/asamwow/Drive")
  (setq org-journal-dir (concat org-directory "/Journal/"))
  (setq org-agenda-files (concat org-directory "/Agenda/")))
(use-package magit)
(use-package dash)

;;; global settings
(menu-bar-mode -1)
(cua-mode 1)
(setq scroll-preserve-screen-position t)

;;; cc-mode
;; (add-hook 'cc-mode 'display-line-numbers-mode)
