;; Google this
(use-package google-this
  :ensure t
  :pin melpa-stable
  :config (google-this-mode 1)
  :bind ("C-c /" . google-this-mode-submap))

(use-package wgrep
  :ensure t)

;; Silver searcher
(use-package ag
  :ensure t
  :config (progn
            (setq ag-highlight-search t)
            (use-package wgrep-ag
              :ensure t)
            )
  :bind (("C-c 1" . ag-regexp)
         ("C-c 2" . ag-project)
         ("C-c 3" . ag-project-at-point)
         ("C-c 4" . ag-regexp-project-at-point)
         ))

;; rgrep
(define-key global-map "\C-x\C-r" 'rgrep)

;; anzu
;; Show number of matches while searching
(use-package anzu
  :ensure t
  :pin melpa-stable
  :config
  (global-anzu-mode t)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace)))

(provide 'init-search)
