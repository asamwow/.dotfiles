;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(cua-mode)

(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(defun my-increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun my-decrement-number-decimal (&optional arg)
  (interactive "p*")
  (my-increment-number-decimal (if arg (- arg) -1)))

(map! ;;"s-`" #'other-frame  ; fix frame-switching
 "C-S-w"         #'delete-window
 "C-w"           #'kill-current-buffer
 "M-w"           #'delete-frame
 "C-n"           #'+default/new-buffer
 "M-n"           #'make-frame
 "C-q"           #'delete-frame
 "C-S-q"         #'kill-emacs
 "C-z"           #'undo
 "C-S-z"         #'redo
 "C-s"           #'save-buffer
 "C-S-s"         #'write-file
 "C-o"           #'find-file
 "C-S-o"         #'revert-buffer
 "C-f"           #'isearch-forward
 "C-S-f"         #'isearch-backward
 "M-+" (λ! (text-scale-set 0))
 "M-="           #'text-scale-increase
 "M--"           #'text-scale-decrease
 "C-="           #'my-increment-number-decimal
 "C--"           #'my-decrement-number-decimal
 "C-a"           #'mark-whole-buffer
 "M-a"           #'select-current-line
 "S-<return>"    #'+default/newline-below
 "C-S-<return>"  #'+default/newline-above
 "M-<backspace>" #'doom/backward-kill-to-bol-and-indent
 "M-<left>"      #'doom/backward-to-bol-or-indent
 "M-<right>"     #'doom/forward-to-last-non-comment-or-eol
 "C-<backspace>" #'backward-kill-word
 "C-<left>"      #'backward-word
 "C-<right>"     #'forward-word
 "M-<up>"        #'scroll-down-line
 "M-<down>"      #'scroll-up-line
 "C-<prior>"     #'beginning-of-buffer
 "C-<next>"      #'end-of-buffer
 "M-<next>"      #'next-buffer
 "M-<prior>"     #'previous-buffer
 "C-;"           #'comment-line
 "C-<del>"       #'kill-word
 "C-r"           #'rotate-layout
 "M-m"           #'magit
 "M-e"           #'eshell
 "M-t"           #'ansi-term
 "C-<tab>"       #'next-multiframe-window
 "M-r"           #'neotree
 )

(setq doom-font (font-spec :family "xos4 Terminus" :size 18))

(setq org-latex-pdf-process
  '("xelatex -interaction nonstopmode %f"
     "xelatex -interaction nonstopmode %f")) ;; for multiple passes

(yas-global-mode 1)

(setq omnisharp-server-executable-path "~/.emacs.d/omnisharp/run")

(eval-after-load
  'company
  '(add-to-list 'company-backends #'company-omnisharp))

(defun my-csharp-mode-setup ()
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)

  ;csharp-mode README.md recommends this too
  ;(electric-pair-mode 1)       ;; Emacs 24
  ;(electric-pair-local-mode 1) ;; Emacs 25

  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)

;(setq org-support-shift-select t)
;(setq org-replace-disputed-keys t)

(setq select-enable-clipboard t)
(setq select-enable-primary nil)
(setq mouse-drag-copy-region nil)

(add-to-list 'default-frame-alist '(height . 48))
(add-to-list 'default-frame-alist '(width . 120))

(setq org-directory "/home/asamwow/Drive")
(setq org-journal-dir (concat org-directory "/Journal/"))
(setq org-agenda-files (concat org-directory "/Agenda/"))
(define-key global-map "\C-cc" 'org-capture)

(setq doom-neotree-enable-variable-pitch nil)

;(add-to-list 'org-latex-packages-alist '("" "listings" nil))
(setq org-latex-listings t)
(setq org-latex-listings-options '(("breaklines" "true")))
