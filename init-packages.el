;; Whole-line or region
(require 'whole-line-or-region)
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

(global-set-key (kbd "C-c n") 'duplicate-line-or-region)

;; Visual regexp
(require-package 'visual-regexp)
(require-package 'visual-regexp-steroids)
(define-key global-map (kbd "M-&") 'vr/query-replace)

;; Theme changer
(require 'theme-changer)
(setq calendar-location-name "Dallas, TX")
(setq calendar-latitude 52.49)
(setq calendar-longitude 13.34)
(require-package 'theme-changer)
(change-theme 'solarized-light 'solarized-dark)

;; Window-Jump
(require-package 'window-jump)
(global-set-key (kbd "C-2") 'window-jump-down)
(global-set-key (kbd "C-8") 'window-jump-up)
(global-set-key (kbd "C-4") 'window-jump-left)
(global-set-key (kbd "C-6") 'window-jump-right)

(global-set-key (kbd "S-<down>") 'window-jump-down)
(global-set-key (kbd "S-<up>") 'window-jump-up)
(global-set-key (kbd "S-<left>") 'window-jump-left)
(global-set-key (kbd "S-<right>") 'window-jump-right)

;; ssh
(require-package 'ssh)

;; Writegood
(require-package 'writegood-mode)
(global-set-key "\C-cw" 'writegood-mode)

;; crontab-mode
(require-package 'crontab-mode)

;; Projectile
(require-package 'projectile)
(projectile-global-mode t)

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

;; fasd in emacs
;; (global-set-key (kbd "C-c f") 'fasd-find-file)
;; (global-fasd-mode 1)

(require 'thingatpt)
(require 'imenu)
(require 'wc)
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

(require-package 'diminish)

;; show free bindings in current buffer
(require-package 'free-keys)

;; guide-key
(require-package 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x v" "C-c"))
(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)

(provide 'init-packages)
