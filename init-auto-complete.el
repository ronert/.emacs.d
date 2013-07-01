(require-package 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(add-to-list 'ac-dictionary-directories (expand-file-name "auto-complete" dotfiles-dir))
(add-to-list 'ac-dictionary-directories "~/.emacs.d/local-autocomplete")
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

(provide 'init-auto-complete)
