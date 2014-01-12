(require-package 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)

;; Override default flycheck triggers
(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
flycheck-idle-change-delay 0.8)

(require-package 'flycheck-tip)
(global-set-key (kbd "C-c C-n") 'flycheck-tip-cycle)
(setq flycheck-tip-avoid-show-func nil)

(provide 'init-flycheck)
