(use-package stan-mode
  :ensure t
  :pin melpa-stable
  :config
  (use-package stan-snippets
    :ensure t
    :pin melpa-stable
    )
  )

(provide 'init-stan)
