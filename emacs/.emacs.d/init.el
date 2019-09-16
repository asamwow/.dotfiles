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
  (setq graphene-default-font "SauceCodePro Nerd Font Mono-11")
  (setq graphene-variable-pitch-font "xos4 Terminus-10")
  (setq graphene-fixed-pitch-font "xos4 Terminus-10"))
(use-package speed-type)
(use-package password-store)
(use-package auth-source-pass
  :init
  (auth-source-pass-enable)
  (setq auth-sources '(password-store))
  (setq auth-source-debug t))
(use-package notmuch
  :init
  (setq mail-user-agent 'message-user-agent)
  (setq user-mail-address "samueljahnke6@gmail.com"
        user-full-name "samueljahnke6")
  (setq smtpmail-smtp-server "smtp.gmail.com"
        message-send-mail-function 'message-smtpmail-send-it)
  (setq smtpmail-debug-info t)
  (setq message-default-mail-headers "Cc: \nBcc: \n")
  (setq message-auto-save-directory "~/mail/draft")
  (setq message-kill-buffer-on-exit t)
  (setq message-directory "~/Mail/"))
(use-package color-theme-sanityinc-tomorrow
  :config
  (color-theme-sanityinc-tomorrow--define-theme night))
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
(use-package csharp-mode
  :init
  (defun my-csharp-mode-hook ()
    ;; enable the stuff you want for C# here
    (electric-pair-mode 1)       ;; Emacs 24
    (electric-pair-local-mode 1) ;; Emacs 25
    )
  (add-hook 'csharp-mode-hook 'my-csharp-mode-hook))

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
;; (add-to-list 'org-babel-load-languages '(python . t))
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  org-babel-load-languages)

;;; latex
(setq-default TeX-engine 'xetex)
(setq-default TeX-PDF-mode t)


;;; org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-support-shift-select t)
