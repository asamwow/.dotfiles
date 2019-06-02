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
(use-package speed-type)
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
(use-package markdown-mode
 :commands (markdown-mode gfm-mode)
 :mode (("README\\.md\\'" . gfm-mode)
        ("\\.md\\'" . markdown-mode)
        ("\\.markdown\\'" . markdown-mode))
 :init (setq markdown-command "multimarkdown"))
(use-package clang-format
  :init
  (global-set-key (kbd "C-c i") 'clang-format-region)
  (global-set-key (kbd "C-c u") 'clang-format-buffer))
(use-package glsl-mode)
(use-package pandoc-mode)
(use-package pdf-tools
  :load-path "/home/asamwow/.emacs.d/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

;;; global settings
(menu-bar-mode -1)
(cua-mode 1)
(setq scroll-preserve-screen-position t)
;; (pdf-loader-install)
;; (pdf-tools-install) ;;change to this to front load it
(setq company-idle-delay 10000)
(global-set-key (kbd "C-c C-<tab>") 'company-complete)

;;; cc-mode
;; (add-hook 'cc-mode 'display-line-numbers-mode)
(setq c-default-style "linux" c-basic-offset 3)
(setq clang-format-style-option "file")

;;; babel
(add-to-list 'org-babel-load-languages '(python . t))
(org-babel-do-load-languages
 'org-babel-load-languages
 org-babel-load-languages)

;;; latex
(setq-default TeX-engine 'xetex)
(setq-default TeX-PDF-mode t)
