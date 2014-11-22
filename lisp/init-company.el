(require-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; latex completion
(require-package 'company-auctex)
(company-auctex-init)
(require-package 'company-math)
(add-to-list 'company-backend 'company-math-symbols-unicode)
(add-to-list 'company-backend 'company-math-symbols-latex)
(add-to-list 'company-backend 'company-latex-commands)

;; haskell completion
(require-package 'company-ghc)
(add-to-list 'company-backends 'company-ghc)

;; python completion
(require-package 'company-anaconda)
(add-to-list 'company-backends 'company-anaconda)

;; scql completion
(require-package 'company-edbi)
(add-to-list 'company-backends 'company-edbi)

;; r completion
(require 'company-ess)

(global-set-key (kbd "C-.") 'company-complete)

(provide 'init-company)
