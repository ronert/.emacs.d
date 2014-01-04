(require-package 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)

(setq-default ac-expand-on-auto-complete nil)
;; (setq-default ac-auto-start nil)
(setq-default ac-dwim nil) ; To get pop-ups with docs even if a word is uniquely completed

(setq tab-always-indent 'complete)  ;; use 't when auto-complete is disabled
(add-to-list 'completion-styles 'initials t)

;; hook AC into completion-at-point
(defun sanityinc/auto-complete-at-point ()
  (when (and (not (minibufferp))
             (fboundp 'auto-complete-mode)
             auto-complete-mode)
    (auto-complete)))

(defun sanityinc/never-indent ()
  (set (make-local-variable 'indent-line-function) (lambda () 'noindent)))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions
        (cons 'sanityinc/auto-complete-at-point
              (remove 'sanityinc/auto-complete-at-point completion-at-point-functions))))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(set-default 'ac-sources
             '(ac-source-imenu
               ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-words-in-all-buffer))

(dolist (mode '(magit-log-edit-mode
                log-edit-mode org-mode text-mode haml-mode
                git-commit-mode
                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                html-mode nxml-mode sh-mode smarty-mode clojure-mode
                lisp-mode textile-mode markdown-mode tuareg-mode
                js3-mode css-mode less-css-mode sql-mode
                sql-interactive-mode
                inferior-emacs-lisp-mode))
  (add-to-list 'ac-modes mode))


;; Exclude very large buffers from dabbrev
(defun sanityinc/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))

(setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)

(add-to-list 'ac-dictionary-directories (expand-file-name "auto-complete" dotfiles-dir))
(add-to-list 'ac-dictionary-directories "~/Dropbox/dotfiles/.emacs.d/local-autocomplete")
;; (setq ac-modes (append ac-modes '(org-mode)))
;; (ac-config-default)
;; (define-key ac-complete-mode-map [tab] 'ac-expand)
;; (setq ac-auto-start 2)
;; (ac-flyspell-workaround)
;; (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
;; (setq ac-auto-show-menu 0.2)
;;
(set-face-background 'ac-candidate-face "#366060")
(set-face-foreground 'ac-selection-face "#1f1f1f")
(set-face-background 'ac-selection-face "#8cd0d3")
(set-face-foreground 'ac-selection-face "#1f1f1f")

(global-set-key (kbd "C-.") 'auto-complete)

;; ac-ispell
(require-package 'ac-ispell)
(setq ac-ispell-requires 4)
(eval-after-load "auto-complete"
  '(progn
     (ac-ispell-setup)))

(defun my/enable-ac-ispell ()
  (add-to-list 'ac-sources 'ac-source-ispell))

(add-hook 'git-commit-mode-hook 'my/enable-ac-ispell)
(add-hook 'latex-mode-hook 'my/enable-ac-ispell)
(add-hook 'org-mode-hook 'my/enable-ac-ispell)

(setq c-tab-always-indent nil
      c-insert-tab-function 'indent-for-tab-command)

(provide 'init-auto-complete)
