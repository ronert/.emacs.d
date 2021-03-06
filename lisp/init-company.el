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

            ;; sql completion
            (use-package company-edbi
              :ensure t
              :config (add-to-list 'company-backends 'company-edbi))

            ;; web completion
            (use-package company-web
              :ensure t
              :config
              (progn
                (add-to-list 'company-backends 'company-web-html)
                (add-to-list 'company-backends 'company-web-jade)
                (add-to-list 'company-backends 'company-web-slim)
                )
              )

            (use-package company-tern
              :ensure t
              :config (add-to-list 'company-backends 'company-tern))

            ;; sort complections by usage statistics
            (use-package company-statistics
              :ensure t
              :config (company-statistics-mode))

              (setq company-etags-everywhere '(php-mode html-mode web-mode nxml-mode))

              ;; company-quickhelp
              ;; (use-package company-quickhelp
              ;;   :ensure t
              ;;   :config (company-quickhelp-mode 1))

              (add-hook 'after-init-hook 'global-company-mode)
              )
            ;; Set a global completion hotkey
            :bind ("C-." . company-complete)
            )

  (provide 'init-company)
