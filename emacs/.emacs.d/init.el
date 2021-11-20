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
(use-package org-contacts
  :ensure nil
  :after org
  :custom (org-contacts-files '("~/.contacts/contacts.org")))
(use-package org-capture
  :ensure nil
  :after org)
(use-package org-alert
  :init (setq alert-default-style 'libnotify)
  (setq org-alert-interval 900))
(use-package graphene
  :init (setq graphene-default-font "SauceCodePro Nerd Font Mono-18")
  (setq graphene-variable-pitch-font "xos4 Terminus-18")
  (setq graphene-fixed-pitch-font "xos4 Terminus-18"))
(use-package speed-type)
(use-package password-store)
(use-package auth-source-pass
  :init (auth-source-pass-enable)
  (setq auth-sources '(password-store))
  (setq auth-source-debug t))
(use-package notmuch)
(use-package color-theme-sanityinc-tomorrow
  :config (color-theme-sanityinc-tomorrow--define-theme night)
  :init (color-theme-sanityinc-tomorrow-night))
(use-package magit
  :init (setq magit-display-buffer-function
              #'magit-display-buffer-fullframe-status-v1)
  (global-set-key (kbd "C-c m") 'magit)
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))
(use-package dash)
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(use-package clang-format
  :init (global-set-key (kbd "C-c i") 'clang-format-region)
  (global-set-key (kbd "C-c u") 'clang-format-buffer))
(use-package glsl-mode)
(use-package pandoc-mode)
(use-package pdf-tools
  :load-path "/home/asamwow/.emacs.d/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :config (pdf-tools-install :no-query))
(use-package csharp-mode)
(use-package ledger-mode
  :mode "\\.ledger\\'")
(use-package omnisharp
:init (eval-after-load 'company
'(add-to-list 'company-backends 'company-omnisharp))
(add-hook 'csharp-mode-hook #'company-mode)
(setq omnisharp-server-executable-path "/home/asamwow/.emacs.d/omnisharp/run")
(setq omnisharp-debug t))
(use-package git-timemachine)
(use-package undo-tree)
(use-package aggressive-indent
  :init (global-set-key (kbd "C-c C-=") 'aggressive-indent-mode))
(use-package rainbow-delimiters)
(use-package column-enforce-mode
  :init (setq column-enforce-column 80)
  (global-set-key (kbd "C-c C-`") 'column-enforce-mode))
(use-package diminish
  :config
  (diminish 'flycheck-mode)
  (diminish 'company-mode)
  (diminish 'eldoc-mode)
  (diminish 'smartparens-mode)
  (diminish 'visual-line-mode)
  (diminish 'column-enforce-mode)
  (diminish 'abbrev-mode))
(use-package js2-mode)
(use-package vue-mode)
(use-package expand-region)
(use-package google-this)
;; (use-package org-tempo)

;; only use plant uml if you have the jar
(let ((plantumljarpath (format "%s/Downloads/plantuml.jar" (getenv "HOME"))))
  (if (file-exists-p plantumljarpath)
      (use-package plantuml-mode
        :init (setq plantuml-jar-path plantumljarpath)
        (setq plantuml-default-exec-mode 'jar))))

(add-to-list 'load-path "/home/asamwow/csharp-mode/")
(require 'csharp-mode)

;;; set global defaults
(menu-bar-mode -1)
(cua-mode 1)
(global-visual-line-mode 0)
(setq-default truncate-lines t)
(setq-default auto-hscroll-mode t)
(setq truncate-partial-width-windows t)
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
(setq vc-follow-symlinks t)
(setq scroll-preserve-screen-position t)
(setq-default fill-column 80)
(setq explicit-shell-file-name "/bin/zsh")
(setq ring-bell-function 'ignore)

;;; minor mode hooks
(defun custom-text-hook ()
  (electric-indent-mode 0)
  (abbrev-mode 1))
(add-hook 'sql-mode-hook #'custom-text-hook)
(defun custom-coding-hook ()
  (custom-text-hook)
  (rainbow-delimiters-mode 1)
  (aggressive-indent-mode 0))
(add-hook 'emacs-lisp-mode-hook #'custom-coding-hook)
(add-hook 'js2-mode-hook #'custom-coding-hook)
(add-hook 'javascript-mode-hook #'custom-coding-hook)
(add-hook 'ledger-mode-hook #'custom-coding-hook)
(defun custom-python-hook ()
  (custom-coding-hook)
  (aggressive-indent-mode 0))
(add-hook 'python-mode-hook #'custom-python-hook)
(defun custom-org-hook ()
  (custom-coding-hook)
  (auto-fill-mode 1))
(add-hook 'org-mode-hook #'custom-org-hook)
(defun custom-message-hook ()
  (flyspell-mode 1))
(add-hook 'notmuch-message-mode-hook #'custom-message-hook)

(setq c-basic-offset 4
      c-tab-always-indent t)
(c-add-style "my-c-style" '((c-continued-statement-offset 4))) ; If a statement continues on the next line, indent the continuation by 4
(defun custom-c-mode-hook ()
  (custom-coding-hook)
  (c-set-style "my-c-style")
  (c-set-offset 'substatement-open '0) ; brackets should be at same indentation level as the statements they open
  (c-set-offset 'inline-open '0)
  (c-set-offset 'block-open '+)
  (c-set-offset 'brace-list-open '+)   ; all "opens" should be indented by the c-indent-level
  (c-set-offset 'case-label '+))       ; indent case labels by c-indent-level, too
(add-hook 'c-mode-hook 'custom-c-mode-hook)
(add-hook 'c++-mode-hook 'custom-c-mode-hook)
(defun custom-csharp-mode-hook ()
  (custom-c-mode-hook)
  (setq c-basic-offset 3))
(add-hook 'csharp-mode-hook #'custom-csharp-mode-hook)


;;; Company Mode
(setq company-idle-delay 10000)
(global-set-key (kbd "C-c C-<tab>") 'company-complete)

;;; babel
(org-babel-do-load-languages 'org-babel-load-languages '((ledger . t)
                                                         (python . t)
                                                         (shell . t)))

;;; latex
(setq-default TeX-engine 'xetex)
(setq-default TeX-PDF-mode t)
(put 'scroll-left 'disabled nil)

;;; auto-commit macro
(defun autocommit-schedule-commit (dn shouldPush commitMessage)
  "Schedule an autocommit (and push) if one is not already
scheduled for the given dir."
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
  (commit-file (read-string "Enter Commit Message: ")))
(defun commit-and-push-file-prompt ()
  "promps user for commit message"
  (interactive)
  (commit-and-push-file (read-string "Enter Commit Message: ")))
(global-set-key (kbd "<f5>") 'autocommit-file)
(global-set-key (kbd "<C-f5>") 'autocommit-and-push-file)
(global-set-key (kbd "<f6>") 'commit-file-prompt)
(global-set-key (kbd "<C-f6>") 'commit-and-push-file-prompt)

;;; javascript
(add-to-list 'auto-mode-alist '("\\.ts\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . javascript-mode))

;;; Shorten Git in modeline
(defun my-shorten-vc-mode-line (string)
  (cond ((string-prefix-p "Git" string)
         (concat " " (substring string 4 (min 23 (length string)))))
        (t string)))
(advice-add 'vc-git-mode-line-string
            :filter-return 'my-shorten-vc-mode-line)

;;; delete trailing whitespaces on save and use unix line endings
(defun delete-carrage-returns ()
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward "\r" nil :noerror)
      (replace-match ""))))
(add-hook 'before-save-hook
          'delete-trailing-whitespace)
(add-hook 'before-save-hook
          'delete-carrage-returns)


;;; increment number at point
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))
(global-set-key (kbd "C-c +") 'increment-number-at-point)

;;; mail
(setq message-sendmail-f-is-evil 't)
(setq sendmail-program "/usr/bin/msmtp")

(defun cg-feed-msmtp ()
  (if (message-mail-p)
      (save-excursion
        (let* ((from (save-restriction (message-narrow-to-headers)
                                       (message-fetch-field "from")))
               (account (cond
                         ;; I use email address as account label in ~/.msmtprc
                         ((string-match "samueljahnke6@gmail.com" from) "pro-gmail")
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

;; ;;; notmuch notifications based from notmuch-unread-mode
;; (defvar notmuch-unread-mode-line-string "")
;; (defvar notmuch-unread-email-count nil)
;; (defconst my-mode-line-map (make-sparse-keymap))
;; (defun notmuch-unread-count ()
;;   (setq notmuch-unread-email-count
;;         (string-to-number(replace-regexp-in-string
;;                           "\n" "" (notmuch-command-to-string
;;                                    "count" "tag:inbox"))))
;;   (if (eq notmuch-unread-email-count 0)
;;       (setq notmuch-unread-mode-line-string
;;             (format "  %d" (string-to-number(replace-regexp-in-string
;;                                               "\n" "" (notmuch-command-to-string
;;                                                        "count")))))
;;     (setq notmuch-unread-mode-line-string
;;           (format "  %d" notmuch-unread-email-count)))
;;   (force-mode-line-update))
;; (run-at-time nil 5 'notmuch-unread-count)
;; (defun notmuch-open-emails ()
;;   (interactive)
;;   (if (eq notmuch-unread-email-count 0)
;;       (notmuch-search "*")
;;     (notmuch-search "tag:inbox")))
;; (setq global-mode-string
;;       (append global-mode-string (list '(:eval (propertize
;;                                                 notmuch-unread-mode-line-string
;;                                                 'help-echo "notmuch emails"
;;                                                 'mouse-face 'mode-line-highlight
;;                                                 'local-map my-mode-line-map)))))
;; (define-key my-mode-line-map (vconcat [mode-line down-mouse-1])
;;   (cons "hello" 'notmuch-open-emails))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(defun mark-whole-sexp ()
  "mark entire expression"
  ( interactive)(beginning-of-sexp)
  (mark-sexp))

(defun find-file-from-home () (interactive)
  (cd "~/")
  (call-interactively 'find-file))

(defun learn-replacement (from to)
  (interactive "sEnter from:
sEnter to:")
  (message "From: %s, To: %s" from to)
  ;; (if (y-or-n-p (format "permently repl \"%s\"->\"%s\"?" from to))
      (progn
        (bookmark-jump "nerd-dictation.py")
        (revert-buffer-no-confirm)
        (beginning-of-buffer)
        (if (not ( search " " from))
            (progn
              (search-forward "WORD_REPLACE = {")
              (newline)
              (c-indent-line-or-region)
              (insert (format "\"%s\":\"%s\"," from to))
              )
          (progn
            (search-forward "TEXT_REPLACE_REGEX = (")
            (newline)
            (c-indent-line-or-region)
            (insert (format "(\"\\\\b\" \"%s\" \"\\\\b\", \"%s\")," from to))
            )
          )
        (save-buffer)
        )
    ;; )
  )

(defun learn-emacs-command (command)
  (interactive "sEnter command:")
  (message "Command: %s" command)
  ;; (if (y-or-n-p (format "permently learn \"%s\"?" command))
      (progn
        (bookmark-jump "nerd-dictation.py")
        (revert-buffer-no-confirm)
        (beginning-of-buffer)
        (search-forward "EMACS_COMMANDS = [")
        (newline)
        (c-indent-line-or-region)
        (insert (format "\"%s\"," command))
        (save-buffer)
        )
    ;; )
  )
