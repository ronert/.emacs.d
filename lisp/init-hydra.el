(use-package hydra
  :ensure t
  :config (progn
            (hydra-create "<f2>"
                          '(("g" text-scale-increase)
                            ("l" text-scale-decrease)))
            ))

(provide 'init-hydra)
