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
  :init (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (setq org-support-shift-select t)
  (setq org-clock-persist 'history))
  ;; (setq org-todo-keywords
  ;;       '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))))
(use-package org-contacts
  :ensure nil
  :after org
  :custom (org-contacts-files '("~/.contacts/contacts.org")))
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
   `(("c" "Contact" entry (file+headline "~/Contacts/contacts.org" "Friends"),
      my/org-contacts-template
      :empty-lines 1))))
(use-package org-alert
  :init
  (setq alert-default-style 'libnotify)
  (setq org-alert-interval 900))
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
(use-package notmuch)
(use-package color-theme-sanityinc-tomorrow
  :config
  (color-theme-sanityinc-tomorrow--define-theme night))
(use-package magit
  :init
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
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
(use-package omnisharp
  :init
  (eval-after-load
      'company
    '(add-to-list 'company-backends 'company-omnisharp))
  (add-hook 'csharp-mode-hook #'company-mode)
  (setq omnisharp-server-executable-path "/home/asamwow/.emacs.d/omnisharp/run")
  (setq omnisharp-debug t))
(use-package ledger-mode
  :mode "\\.ledger\\'")
(use-package git-timemachine)
(use-package undo-tree)
(use-package plantuml-mode
  :init
  (setq plantuml-jar-path "/home/asamwow/Downloads/plantuml.jar")
  (setq plantuml-default-exec-mode 'jar))

;;; global settings
(menu-bar-mode -1)
(cua-mode 1)
(setq scroll-preserve-screen-position t)
;; (pdf-loader-install)
;; (pdf-tools-install) ;;change to this to front load it
(setq company-idle-delay 10000)
(global-set-key (kbd "C-c C-<tab>") 'company-complete)
(c-set-offset 'case-label '+)

;;; notmuch notifications based from notmuch-unread-mode
(defvar notmuch-unread-mode-line-string "")
(setq global-mode-string (append global-mode-string 'notmuch-unread-mode-line-string))
(defun notmuch-unread-count ()
  (let ((inboxCount (string-to-number(replace-regexp-in-string "\n" "" (notmuch-command-to-string "count" "tag:inbox")))))
  (if (eq inboxCount 0) (setq notmuch-unread-mode-line-string (format "  %d" (string-to-number(replace-regexp-in-string "\n" "" (notmuch-command-to-string "count"))))) (setq notmuch-unread-mode-line-string (format "  %d" inboxCount)))))
(run-at-time nil 10 'notmuch-unread-count)

;;; cc-mode
(setq c-default-style "linux" c-basic-offset 3)
(setq clang-format-style-option "file")

;;; babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (ledger . t)
   (python . t)
 ))

;;; latex
(setq-default TeX-engine 'xetex)
(setq-default TeX-PDF-mode t)
(put 'scroll-left 'disabled nil)

;;; auto-commit macro
(defun autocommit-schedule-commit (dn shouldPush commitMessage)
  "Schedule an autocommit (and push) if one is not already scheduled for the given dir."
          (message (concat "Committing files in " dn))
          (shell-command (concat "cd " dn " && git commit -m '" commitMessage "'"))
          (cond ((string= shouldPush "t")
               (message (concat "pushing files in " dn))
               (shell-command (concat "cd " dn " && git push -u origin master")))))
(defun commit-and-push-file(commitMessage)
  "'git add' the modified file and schedule a commit and push in the idle loop."
  (let ((fn (buffer-file-name)))
    (message "git adding %s" fn)
    (shell-command (concat "git add " fn))
    (autocommit-schedule-commit (file-name-directory fn) "t" commitMessage)))
(defun commit-file (commitMessage)
  "'git add' the modified file and schedule a commit and push in the idle loop."
  (let ((fn (buffer-file-name)))
    (message "git adding %s" fn)
    (shell-command (concat "git add " fn))
    (autocommit-schedule-commit (file-name-directory fn) "nil" commitMessage)))
(defun autocommit-file ()
  "'git add' the modified file and schedule a commit and push in the idle loop."
  (interactive)
  (commit-file "auto-commit"))
(defun autocommit-and-push-file()
  "'git add' the modified file and schedule a commit and push in the idle loop."
  (interactive)
  (commit-and-push-file "auto-commit-and-push"))
(defun commit-file-prompt ()
  "promps user for commit message"
  (interactive)
  (commit-file (read-string "Enter Commit Message")))
(defun commit-and-push-file-prompt ()
  "promps user for commit message"
  (interactive)
  (commit-and-push-file (read-string "Enter Commit Message")))
(global-set-key (kbd "<f5>") 'autocommit-file)
(global-set-key (kbd "<C-f5>") 'autocommit-and-push-file)
(global-set-key (kbd "<f6>") 'commit-file-prompt)
(global-set-key (kbd "<C-f6>") 'commit-and-push-file-prompt)

;;; javascript
(add-to-list 'auto-mode-alist '("\\.ts\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . javascript-mode))

;;; mail
(setq message-sendmail-f-is-evil 't)
(setq sendmail-program "/usr/bin/msmtp")

(defun cg-feed-msmtp ()
  (if (message-mail-p)
      (save-excursion
    (let* ((from
        (save-restriction
          (message-narrow-to-headers)
          (message-fetch-field "from")))
           (account
        (cond
         ;; I use email address as account label in ~/.msmtprc
         ((string-match "samueljahnke6@gmail.com" from) "pro-gmail")
         ;; Add more string-match lines for your email accounts
         ((string-match "sam.jahnke@hvhprecision.com" from) "hvh"))))
      (setq message-sendmail-extra-arguments (list '"-a" account))))))

(setq message-sendmail-envelope-from 'header)
(add-hook 'message-send-mail-hook 'cg-feed-msmtp)
(setq user-mail-address "sam.jahnke@hvhprecision.com")
(setq user-full-name "Samuel Jahnke")
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq message-default-mail-headers "Cc: \nBcc: \n")
(setq message-auto-save-directory "~/Mail/draft")
(setq message-kill-buffer-on-exit t)
(setq message-directory "~/Mail/")
(setq message-signature "\
Sincerely,
Samuel Jahnke. HVH Precision.
(507) 399-6195
Sent from Emacs!
")

;;; Shorten Git in modeline
(defun my-shorten-vc-mode-line (string)
  (cond
   ((string-prefix-p "Git" string)
    (concat " " (substring string 4 (min 23 (length string)))))
   (t
    string)))
(advice-add 'vc-git-mode-line-string :filter-return 'my-shorten-vc-mode-line)
