(use-package idle-highlight-mode
  :ensure t
  :pin melpa-stable)

;; Whole-line or region
(use-package whole-line-or-region
  :ensure t
  :pin melpa-stable
  :config
  (progn
    (whole-line-or-region-mode t)
    (defun duplicate-region (beg end)
      "Insert a copy of the current region after the region."
      (interactive "r")
      (save-excursion
        (goto-char end)
        (insert (buffer-substring beg end))))

    (defun duplicate-line-or-region (prefix)
      "Duplicate either the current line or any current region."
      (interactive "*p")
      (whole-line-or-region-call-with-region 'duplicate-region prefix t))

    (global-set-key (kbd "C-c n") 'duplicate-line-or-region)))

;; winner mode
(winner-mode 1)
(global-set-key (kbd "C-c <up>") 'winner-undo)
(global-set-key (kbd "C-c <down>") 'winner-redo)


;; smex
(use-package smex
  :ensure t
  :pin melpa-stable
  :config (progn
            (smex-initialize)
            (setq smex-show-unbound-commands t)
            (smex-auto-update 30))
  :bind
  (
   ;;("M-x" . smex) using helm instead for now
   ("C-x C-m" . smex) ;; supersedes binding in starter-kit-bindings.org
   ("M-X" . smex-major-mode-commands)
   ("C-x C-M" . smex-major-mode-commands)
   ;; This is your old M-x.
   ("C-c C-c M-x" . execute-extended-command)
   ("C-x C-m" . smex)
   ))



;; Goto last change
(use-package goto-last-change
  :ensure t
  :pin melpa-stable
  :config (progn  (global-unset-key (kbd "C-+"))
                  (global-set-key (kbd "C-+") 'goto-last-change)))

(use-package multiple-cursors
  :ensure t
  :pin melpa-stable
  :bind
  (
   ;;When you want to add multiple cursors not based on continuous lines, but based on keywords in the buffer, use:
   ("C-ö" . mc/mark-next-like-this)
   ("C-Ö" . mc/umark-next-like-this)
   ("C-ü" . mc/mark-previous-like-this)
   ("C-Ü" . mc/umark-previous-like-this)
   ("C-ä" . mc/mark-all-like-this)
   ("C-c C-a" . mc/edit-beginnings-of-lines)
   ("C-c C-e" . mc/edit-ends-of-lines)
   ("C-:" . mc/edit-lines)
   )
  )

;; phi search for multiple cursors
(use-package phi-search
  :ensure t
  :bind (
         ("C-c C-)" . phi-search)
         ("C-c C-(" . phi-search-backward)))

(use-package smart-forward
  :ensure t
  :bind (("s-<up>" . smart-up)
         ("s-<down>" . smart-down)
         ("s-<left>" . smart-backward)
         ("s-<right>" . smart-forward)))

(use-package avy
  :ensure t
  :config (progn
            (setq avy-background t)
            (setq avy-goto-char-style 'at-full)
            (setq avy-goto-word-style 'at-full)))

(use-package expand-region
  :bind ("C-#" . er/expand-region))

(use-package key-chord
  :ensure t
  :config
  (progn
    (defun disable-key-chord-mode ()
      (set (make-local-variable 'input-method-function) nil))

    (add-hook 'minibuffer-setup-hook #'disable-key-chord-mode)
    (key-chord-mode 1)
    (key-chord-define-global "xc" "[")
    (key-chord-define-global "bn" "]")
    (key-chord-define-global "yx" "{")
    (key-chord-define-global "nm" "}")
    (key-chord-define-global "nm" "}")
    (key-chord-define-global "qw" "/")
    (key-chord-define-global "ü+" "\\")
    (key-chord-define-global ",," 'hippie-expand)
    (key-chord-define-global ",." 'company-complete)
    ;; avy
    (key-chord-define-global "hh" 'avy-goto-char)
    (key-chord-define-global "jj" 'avy-goto-line)
    (key-chord-define-global "kk" 'avy-goto-word-1)
    ;; lines
    (key-chord-define-global "ii" 'smart-open-line-above)
    (key-chord-define-global "uu" 'open-line-below)
    (key-chord-define-global "zz" 'just-one-space)
    (key-chord-define-global "aa" 'delete-horizontal-space)
    ;; add some neat keychords
    (key-chord-define-global "0o" "=")
    (key-chord-define-global "1q" "!")
    (key-chord-define-global "2w" "\"")
    (key-chord-define-global "3e" "#")
    (key-chord-define-global "4r" "$")
    (key-chord-define-global "5t" "%")
    (key-chord-define-global "6z" "^")
    (key-chord-define-global "6t" "&")
    (key-chord-define-global "7z" "/")
    (key-chord-define-global "8u" "(")
    (key-chord-define-global "9i" ")")
    (key-chord-define-global "ßp" "?")
    ;; When you have an active region that spans multiple lines, the following will add a cursor to each line:
    (key-chord-define-global "vv" 'mc/edit-lines)
    (autoload 'zap-up-to-char "misc")
    ;;  Kill up to, but not including ARGth occurrence of CHAR
    (key-chord-define-global "ää" 'zap-up-to-char)
    (key-chord-define-global "üü" 'zap-to-char)
    ))

;; Visual regexp
(use-package visual-regexp
  :ensure t
  :config (define-key global-map (kbd "M-&") 'vr/query-replace))
(use-package visual-regexp-steroids
  :ensure t)

;; Window-Jump
(use-package window-jump
  :ensure t
  :bind
  (("C-2" . window-jump-down)
   ("C-8" . window-jump-up)
   ("C-4" . window-jump-left)
   ("C-6" . window-jump-right)
   ("S-<down>" . window-jump-down)
   ("S-<up>" . window-jump-up)
   ("S-<left>" . window-jump-left)
   ("S-<right>" . window-jump-right)))

;; ssh
(use-package ssh
  :ensure t)

;; Writegood
(use-package writegood-mode
  :ensure t
  :pin melpa-stable
  :bind ("\C-cw" . writegood-mode))

;; crontab-mode
(use-package crontab-mode
  :ensure t)

;; Projectile
(use-package helm-projectile
  :ensure t)
(use-package projectile
  :ensure t
  :pin melpa-stable
  :bind ("C-;" . projectile-helm-ag)
  :config
  (progn
    (projectile-global-mode t)
    (helm-projectile-on)
    (defun projectile-helm-ag ()
      (interactive)
      (helm-ag (projectile-project-root)))
    (setq projectile-completion-system 'helm)
    ))


;; saner regex syntax
(use-package re-builder
  :ensure t
  :config (setq reb-re-syntax 'string))

;; fasd in emacs
(use-package fasd
  :ensure t
  :config (global-fasd-mode 1)
  :bind ("C-c f" . fasd-find-file)
  )

;; Make a scratch buffer for any major mode
(use-package scratch
  :ensure t
  :config)

(use-package thingatpt
  :ensure t)
(use-package imenu
  :ensure t)
(use-package cl
  :ensure t)
(use-package saveplace
  :ensure t)
(use-package ffap
  :ensure t)
(use-package ansi-color
  :ensure t)
(use-package recentf
  :ensure t)

;; show free bindings in current buffer
(use-package free-keys
  :ensure t)

;; Discover my major
(use-package discover-my-major
  :ensure t
  :bind ("C-h C-m" . discover-my-major))

;; Ace-Link http://irreal.org/blog/?p=2594
(use-package ace-link
  :ensure t
  :pin melpa-stable
  :config (ace-link-setup-default))

(use-package hungry-delete
  :ensure t
  :config (global-hungry-delete-mode))

;; Dash
(when *is-a-mac*
  (require-package 'dash-at-point)
  (global-set-key (kbd "C-c D") 'dash-at-point))

;; Dash-at-point
(use-package dash-at-point
  :ensure t)

;; Restclient
(use-package restclient
  :ensure t
  :config (use-package company-restclient
            :ensure t
            :config (add-to-list 'company-backends 'company-restclient)))

;; Browse Kill Ring
;; (use-package browse-kill-ring
;;   :ensure t
;;   :pin melpa-stable)


;; Enhance package.el
(use-package paradox
  :ensure t)

(use-package bug-hunter
  :ensure t)

(provide 'init-packages)
