(use-package company
  :ensure t
  :pin melpa-stable
  :config (progn
            ;; latex completion
            (use-package company-auctex
              :ensure t
              :config
              (progn
                (company-auctex-init)
                (require-package 'company-math)
                (add-to-list 'company-backend 'company-math-symbols-unicode)
                (add-to-list 'company-backend 'company-math-symbols-latex)
                (add-to-list 'company-backend 'company-latex-commands)))

            ;; haskell completion
            (use-package company-ghc
              :ensure t
              :pin melpa-stable
              :config (add-to-list 'company-backends 'company-ghc))

            ;; python completion
            (use-package company-anaconda
              :ensure t
              :config (add-to-list 'company-backends 'company-anaconda))

            ;; sql completion
            (use-package company-edbi
              :ensure t
              :config (add-to-list 'company-backends 'company-edbi))

            ;; r completion
            (use-package company-ess
              :ensure t
              :config (add-to-list 'company-backends 'company-ess-backend))

            (add-hook 'after-init-hook 'global-company-mode)
            )
  ;; Set a global completion hotkey
  :bind ("C-." . company-complete)
  )

(provide 'init-company)
