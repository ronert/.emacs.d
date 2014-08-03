(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require-package 'company-auctex)
(company-auctex-init)

(add-to-list 'company-backends 'company-ghc)
(add-to-list 'company-backends 'company-anaconda)

(provide 'init-company)
