(require-package 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(add-to-list 'ac-dictionary-directories (expand-file-name "auto-complete" dotfiles-dir))
(add-to-list 'ac-dictionary-directories "/Users/ronert/Dropbox/dotfiles/.emacs.d/local-autocomplete")
(setq ac-modes (append ac-modes '(org-mode)))
(ac-config-default)
(define-key ac-complete-mode-map [tab] 'ac-expand)
(setq ac-auto-start 2)
(ac-flyspell-workaround)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(setq ac-auto-show-menu 0.2)
;;
(set-face-background 'ac-candidate-face "#366060")
(set-face-foreground 'ac-selection-face "#1f1f1f")
(set-face-background 'ac-selection-face "#8cd0d3")
(set-face-foreground 'ac-selection-face "#1f1f1f")

;; auto-complete
(global-set-key (kbd "C-.") 'hippie-expand)

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

(provide 'init-auto-complete)
