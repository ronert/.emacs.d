(use-package helm
  :ensure t
  :pin melpa-stable
  :bind ("C-c h" . helm-mini)
  :config
  ;; activate helm-descbinds
  (use-package helm-descbinds
    :ensure t
    :pin melpa-stable
    :init (helm-descbinds-mode))
  ;; activate helm-R
  (use-package helm-R
    :ensure t)
  ;; activate helm-R
  (use-package helm-ack
    :ensure t)
  (use-package helm-themes
    :ensure t
    :pin melpa-stable))

(provide 'init-helm)
