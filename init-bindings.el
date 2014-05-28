;; set next error to '
(global-set-key (kbd "M-'") 'next-error)

;; Move text
(global-set-key (kbd "C-S-<right>") 'es-move-text-right)
(global-set-key (kbd "C-S-<left>") 'es-move-text-left)
(global-set-key (kbd "C-S-<up>") 'es-move-text-up)
(global-set-key (kbd "C-S-<down>") 'es-move-text-down)

(global-set-key (kbd "C-c o") 'open-with)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)
(global-set-key (kbd "M-T") 'transpose-lines)
(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)
(global-set-key [remap goto-line] 'goto-line-with-feedback)
(global-set-key [remap kill-whole-line] 'smart-kill-whole-line)
(global-set-key (kbd "C-M-z") 'indent-defun)
;; global key for `multi-occur-in-this-mode' - you should change this.
(global-set-key (kbd "C-c r") 'multi-occur-in-this-mode)

;; Display line numbers with keybinding C-F6
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
(global-set-key (kbd "C-<f6>") 'linum-mode)

(global-set-key (kbd "C-c i") 'prelude-ido-goto-symbol)

(global-set-key "\C-x2" (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key "\C-x3" (split-window-func-with-other-buffer 'split-window-horizontally))

(global-set-key "\C-x|" 'split-window-horizontally-instead)
(global-set-key "\C-x_" 'split-window-vertically-instead)

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(global-set-key (kbd "C-z") 'yank)
(global-set-key (kbd "M-z") 'yank-pop)

(global-set-key (kbd "<kp-1>") 'delete-other-windows)

(global-set-key (kbd "C-x w") 'rename-current-buffer-file)

(global-set-key (kbd "<M-S-down>") 'move-line-down)
(global-set-key (kbd "<M-S-up>") 'move-line-up)


(global-set-key (kbd "C-x //") 'align-regexp)

(global-set-key (kbd "M-ß") 'prelude-cleanup-buffer)

;; A complementary binding to the apropos-command(C-h a)
(global-set-key (kbd "C-h A") 'apropos)


;; Create new frame (bound to regular mac-command)
(define-key global-map (kbd "C-x C-n") 'make-frame-command)

;; Window switching
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x -") 'rotate-windows)

;; Query replace regex key binding
(global-set-key (kbd "M-&") 'query-replace-regexp)
(global-set-key (kbd "C-c q") 'query-replace)

;; Comment out block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
;; Uncomment block
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Mark all
(global-set-key (kbd "C-c m") 'mark-whole-buffer)

;; Browse the kill ring
(global-set-key (kbd "C-x z") 'browse-kill-ring)

;; cleanup-buffer
(global-set-key (kbd "C-c ß") 'cleanup-buffer)

;; resizing 'windows' (i.e., inside the frame)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Add keybindings for commenting regions of text
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

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


;; When you have an active region that spans multiple lines, the following will add a cursor to each line:
(key-chord-define-global "vv" 'mc/edit-lines)

(autoload 'zap-up-to-char "misc")
;;  Kill up to, but not including ARGth occurrence of CHAR
(key-chord-define-global "ää" 'zap-up-to-char)
(key-chord-define-global "üü" 'zap-to-char)

;; iy-goto-char
(global-set-key (kbd "M-m") 'iy-go-to-char)


;; Move more quickly
(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))


(global-set-key (kbd "C-x //") 'align-regexp)

(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)


(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)

(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))

(windmove-default-keybindings)
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two
(setq windmove-wrap-around t)

;; resizing 'windows' (i.e., inside the frame)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "C-x a") 'join-line)
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(global-set-key [remap backward-up-list] 'backward-up-sexp)

(global-set-key (kbd "C-t") 'eshell)

(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

(global-set-key (kbd "C-x M-m") 'shell)

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

(global-set-key (kbd "C-x h") 'view-url)

(global-set-key (kbd "C-h a") 'apropos)

(global-set-key (kbd "C-c e") 'eval-and-replace)

(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(winner-mode 1)
(global-set-key (kbd "C-c <up>") 'winner-undo)
(global-set-key (kbd "C-c <down>") 'winner-redo)

(setq cua-enable-cua-keys nil)
(cua-mode)


(global-set-key (kbd "RET") 'newline-and-indent)
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "<M-return>") 'sanityinc/newline-at-end-of-line)

(provide 'init-bindings)
