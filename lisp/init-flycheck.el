(use-package flycheck
  :ensure t
  :pin melpa-stable
  :config
  (progn
    (add-hook 'after-init-hook 'global-flycheck-mode)
    ;; Override default flycheck triggers
    (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
          flycheck-idle-change-delay 0.8)
    (use-package flycheck-tip
      :ensure t
      :bind ("C-c C-n" . flycheck-tip-cycle)
      :config (setq flycheck-tip-avoid-show-func nil))
    ))
(provide 'init-flycheck)
