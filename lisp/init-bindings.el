; Editing
(global-set-key (kbd "M-T") 'transpose-lines)
(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)
(global-set-key [remap kill-whole-line] 'smart-kill-whole-line)
(global-set-key (kbd "C-M-z") 'indent-defun)
;; Remap yank and yank-pop to German keyboard
(global-set-key (kbd "C-z") 'yank)
(global-set-key (kbd "M-z") 'yank-pop)
(global-set-key (kbd "C-x //") 'align-regexp)
(global-set-key (kbd "M-ß") 'prelude-cleanup-buffer)
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
;; Move text
(global-set-key (kbd "C-S-<right>") 'es-move-text-right)
(global-set-key (kbd "C-S-<left>") 'es-move-text-left)
(global-set-key (kbd "C-S-<up>") 'es-move-text-up)
(global-set-key (kbd "C-S-<down>") 'es-move-text-down)
;; Add keybindings for commenting regions of text
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x //") 'align-regexp)
(global-set-key (kbd "C-x a") 'join-line)
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(global-set-key [remap backward-up-list] 'backward-up-sexp)

;; Navigation
(global-set-key (kbd "C-c i") 'prelude-ido-goto-symbol)
(global-set-key [remap goto-line] 'goto-line-with-feedback)
;; set next error to '
(global-set-key (kbd "M-'") 'next-error)
;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)
(global-set-key (kbd "<M-S-down>") 'move-line-down)
(global-set-key (kbd "<M-S-up>") 'move-line-up)
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
;; (global-set-key (kbd "RET") 'newline-and-indent)
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "<M-return>") 'sanityinc/newline-at-end-of-line)
;; iy-goto-char
(global-set-key (kbd "M-m") 'iy-go-to-char)


;; Search
;; global key for `multi-occur-in-this-mode' - you should change this.
(global-set-key (kbd "C-c r") 'multi-occur-in-this-mode)
;; Query replace regex key binding
(global-set-key (kbd "M-&") 'query-replace-regexp)
(global-set-key (kbd "C-c q") 'query-replace)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))


;; Buffer management
(global-set-key "\C-x2" (split-window-func-with-other-buffer 'split-window-vertically))
(global-set-key "\C-x3" (split-window-func-with-other-buffer 'split-window-horizontally))
(global-set-key "\C-x|" 'split-window-horizontally-instead)
(global-set-key "\C-x_" 'split-window-vertically-instead)
(global-set-key (kbd "<kp-1>") 'delete-other-windows)
(global-set-key (kbd "C-x w") 'rename-current-buffer-file)
(global-set-key (kbd "C-c o") 'open-with)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)
;; Create new frame (bound to regular mac-command)
(define-key global-map (kbd "C-x C-n") 'make-frame-command)
;; Window switching
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x -") 'rotate-windows)
;; resizing 'windows' (i.e., inside the frame)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-M-f") 'find-file-in-project)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
;; (when (require 'browse-kill-ring nil 'noerror)
;;   (browse-kill-ring-default-keybindings))
(windmove-default-keybindings)
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two
(setq windmove-wrap-around t)
;; resizing 'windows' (i.e., inside the frame)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)


;; Help
;; A complementary binding to the apropos-command(C-h a)
(global-set-key (kbd "C-h A") 'apropos)
(global-set-key (kbd "C-h a") 'apropos)
;; Bind info-display manual-entry
(define-key 'help-command (kbd "C-i") 'info-display-manual)


;; Misc bindings
;; Display line numbers with keybinding C-F6
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
(global-set-key (kbd "C-<f6>") 'linum-mode)
(global-set-key (kbd "C-t") 'eshell)
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))
(global-set-key (kbd "C-x M-m") 'shell)
(global-set-key (kbd "C-x h") 'view-url)
(global-set-key (kbd "C-c e") 'eval-and-replace)
(setq cua-enable-cua-keys nil)
(cua-mode)

(provide 'init-bindings)
