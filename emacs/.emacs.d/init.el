;;;; init.el

;;; package management
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t))
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'no-error 'no-message)

;;; packages
(use-package org
  :pin gnu
  :init (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (setq org-support-shift-select t)
  (setq org-clock-persist 'history))
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
(use-package expand-region)
(use-package google-this)

;; only use plant uml if you have the jar
(let ((plantumljarpath (format "%s/Downloads/plantuml.jar" (getenv "HOME"))))
  (if (file-exists-p plantumljarpath)
      (use-package plantuml-mode
        :init (setq plantuml-jar-path plantumljarpath)
        (setq plantuml-default-exec-mode 'jar))))

;;; set global defaults
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
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
(setcdr (assoc 'file org-link-frame-setup) 'find-file)
(setq org-confirm-babel-evaluate nil)
(setq garbage-collection-messages t)
(add-to-list 'default-frame-alist
             '(font . "SauceCodePro Nerd Font Mono-14"))
(ido-mode 1)
(blink-cursor-mode 0)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

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
(add-hook 'ledger-mode-hook #'custom-coding-hook)
(defun custom-python-hook ()
  (custom-coding-hook)
  (aggressive-indent-mode 0))
(add-hook 'python-mode-hook #'custom-python-hook)
(defun custom-org-hook ()
  (custom-coding-hook)
  (auto-fill-mode 1)
  (global-set-key (kbd "C-c C-<tab>") 'company-complete))
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
(global-set-key (kbd "<RET>") 'default-indent-new-line)

;;; babel
(org-babel-do-load-languages 'org-babel-load-languages '((ledger . t)
                                                         (python . t)
                                                         (lisp . t)
                                                         (sql . t)
                                                         (shell . t)))

;;; latex
(setq-default TeX-engine 'xetex)
(setq-default TeX-PDF-mode t)
(put 'scroll-left 'disabled nil)

;;; delete trailing whitespaces on save and use unix line endings
(defun my-clean-file ()
  (interactive)
  (if (or (eq major-mode 'lisp-mode) (eq major-mode 'org-mode))
      (progn
        (delete-trailing-whitespace)
        (save-excursion
          (goto-char 0)
          (while (search-forward "\r" nil :noerror)
        (replace-match ""))))))
(add-hook 'before-save-hook
          'my-clean-file)

;;; increment number at point
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))
(global-set-key (kbd "C-c +") 'increment-number-at-point)

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

(defun c-if-statement (expression)
  (interactive "sEnter boolean expression:")
  (message "Expression: %s" expression)
  (progn
    (insert (format "if (%s) {" expression))
    (default-indent-new-line)
    )
  )

(defun duplicate-region ()
  (interactive)
  (beginning-of-line)
  (er/expand-region 1)
  (kill-ring-save (mark) (point))
  (previous-line)
  (end-of-line)
  (default-indent-new-line)
  (yank)
  )

(defun mark-between-empty-lines ()
  (interactive)
  (search-backward-regexp "^$")
  (next-line)
  (set-mark-command nil)
  (search-forward-regexp "^$")
  )

(defun evaluate-definition ()
  (interactive)
  (end-of-defun)
  (eval-last-sexp nil)
  )

(defun evaluate-lisp ()
  (interactive)
  (eval-last-sexp nil)
  )

(defun make-executable ()
  (interactive)
  (shell-command (format "chmod +x %s" (buffer-file-name)))
  )
(defun make-file ()
  (interactive)
  (shell-command (format "make %s" (buffer-file-name)))
  )

(load-file "~/stratagem.el")
