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

;; winner mode
(winner-mode 1)
(global-set-key (kbd "C-c <up>") 'winner-undo)
(global-set-key (kbd "C-c <down>") 'winner-redo)


;; smex
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-x C-m") 'smex) ;; supersedes binding in starter-kit-bindings.org
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-x C-M") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(setq smex-show-unbound-commands t)
(smex-auto-update 30)

(global-set-key (kbd "C-x C-m") 'smex)


;; iy-goto-char
(global-set-key (kbd "M-m") 'iy-go-to-char)

;; Goto last change
(require-package 'goto-last-change)
(global-unset-key (kbd "C-+"))
(global-set-key (kbd "C-+") 'goto-last-change)

(require-package 'multiple-cursors)
;;When you want to add multiple cursors not based on continuous lines, but based on keywords in the buffer, use:
(global-set-key (kbd "C-ö") 'mc/mark-next-like-this)
(global-set-key (kbd "C-ü") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-ä") 'mc/mark-all-like-this)

(require-package 'smart-forward)
(global-set-key (kbd "s-<up>") 'smart-up)
(global-set-key (kbd "s-<down>") 'smart-down)
(global-set-key (kbd "s-<left>") 'smart-backward)
(global-set-key (kbd "s-<right>") 'smart-forward)

(require-package 'ace-jump-buffer)
;;(global-set-key [?\S- ] 'ace-jump-buffer)

(require-package 'ace-jump-mode)

(require-package 'jump-char)

(require-package 'expand-region)
(global-set-key (kbd "C-#") 'er/expand-region)

(require-package 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "xc" "[")
(key-chord-define-global "bn" "]")
(key-chord-define-global "yx" "{")
(key-chord-define-global "nm" "}")
(key-chord-define-global "nm" "}")
(key-chord-define-global "qw" "/")
(key-chord-define-global "ü+" "\\")
(key-chord-define-global "hh" 'jump-char-forward)
(key-chord-define-global "aa" 'jump-char-backward)
(key-chord-define-global "öö" 'iy-go-to-char)
(key-chord-define-global ",," 'hippie-expand)
(key-chord-define-global ",." 'auto-complete)
(key-chord-define-global "jj" 'ace-jump-line-mode)
(key-chord-define-global "kk" 'ace-jump-word-mode)
(key-chord-define-global "ii" 'smart-open-line-above)
(key-chord-define-global "uu" 'open-line-below)
(key-chord-define-global "zz" 'just-one-space)
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


;; Visual regexp
(require-package 'visual-regexp)
(require-package 'visual-regexp-steroids)
(define-key global-map (kbd "M-&") 'vr/query-replace)

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
;;(require 'wc)
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require-package 'highlight-symbol)

;; show free bindings in current buffer
(require-package 'free-keys)

;; Discover my major
(require-package 'discover-my-major)
(global-set-key (kbd "C-h C-m") 'discover-my-major)

;; Ace-Link http://irreal.org/blog/?p=2594
(require-package 'ace-link)
(ace-link-setup-default)

;; activate hungry delete
(unless (fboundp 'hungry-delete-mode)
  (package-install 'hungry-delete))

(require 'hungry-delete)
(global-hungry-delete-mode)

;; Dash
(require-package 'dash)

;; Template
(add-to-list 'load-path "~/.emacs.d/src/template/")
(require 'template)

(provide 'init-packages)
