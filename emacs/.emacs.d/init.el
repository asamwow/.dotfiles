;;;; init.el

;;; package management
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'no-error 'no-message)

;;; packages
(use-package org
  :ensure org-plus-contrib
  ;; The rest of your org-mode configuration
  :init (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (setq org-support-shift-select t))
  ;; (setq org-todo-keywords
  ;;       '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))))
(use-package org-contacts
  :ensure nil
  :after org
  :custom (org-contacts-files '("~/contacts.org")))
(use-package org-capture
  :ensure nil
  :after org
  :preface
  (defvar my/org-contacts-template "* %(org-contacts-template-name)
:PROPERTIES:
:ADDRESS: %^{289 Cleveland St. Brooklyn, 11206 NY, USA}
:BIRTHDAY: %^{yyyy-mm-dd}
:EMAIL: %(org-contacts-template-email)
:NOTE: %^{NOTE}
:END:" "Template for org-contacts.")
  :custom
  (org-capture-templates
   `(("c" "Contact" entry (file+headline "~/contacts.org" "Friends"),
      my/org-contacts-template
      :empty-lines 1))))
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
    (electric-pair-local-mode 1))
  (add-hook 'csharp-mode-hook 'my-csharp-mode-hook))
(use-package ledger-mode
  :mode "\\.ledger\\'")

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
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   ;; (R . t)
   ;; (ditaa . t)
   ;; (dot . t)
   ;; (emacs-lisp . t)
   ;; (gnuplot . t)
   ;; (haskell . nil)
   ;; (latex . t)
   (ledger . t)
   ;; (ocaml . nil)
   ;; (octave . t)
   (python . t)
   ;; (ruby . t)
   ;; (screen . nil)
   ;; (sh . t)
   ;; (sql . nil)
   ;; (sqlite . t)
 ))

;;; latex
(setq-default TeX-engine 'xetex)
(setq-default TeX-PDF-mode t)
(put 'scroll-left 'disabled nil)

;; (eval-after-load 'org
;;   '(progn
;;      (defun wicked/org-clock-in-if-starting ()
;;        "Clock in when the task is marked STARTED."
;;        (when (and (string= state "STARTED")
;; 		  (not (string= last-state state)))
;; 	 (org-clock-in)))
;;      (add-hook 'org-after-todo-state-change-hook
;; 	       'wicked/org-clock-in-if-starting)
;;      (defadvice org-clock-in (after wicked activate)
;;       "Set this task's status to 'STARTED'."
;;       (org-todo "STARTED"))
;;     (defun wicked/org-clock-out-if-waiting ()
;;       "Clock out when the task is marked WAITING."
;;       (when (and (string= state "WAITING")
;;                  (equal (marker-buffer org-clock-marker) (current-buffer))
;;                  (< (point) org-clock-marker)
;; 	         (> (save-excursion (outline-next-heading) (point))
;; 		    org-clock-marker)
;; 		 (not (string= last-state state)))
;; 	(org-clock-out)))
;;     (add-hook 'org-after-todo-state-change-hook
;; 	      'wicked/org-clock-out-if-waiting)))

(defun my/after-change-hook()
  "Test function on hook."
  (message org-state)
  (when (string= org-state "ASLEEP")
    (save-buffer)
    (shell-command "git commit -m \"hello?\"")
    (kill-emacs)
  )
)
(add-hook 'org-after-todo-state-change-hook 'my/after-change-hook)
