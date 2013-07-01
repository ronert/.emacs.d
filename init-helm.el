(require-package 'helm)
(global-set-key (kbd "C-c h") 'helm-mini)
;; activate helm-descbinds
(require-package 'helm-descbinds)
(helm-descbinds-mode)
;; activate helm-R
(require-package 'helm-R)
;; activate helm-R
(require-package 'helm-ack)
(require-package 'helm-themes)

(provide 'init-helm)
