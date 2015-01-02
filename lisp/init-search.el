;; Google this
(use-package google-this
  :ensure t
  :pin melpa-stable
  :init (google-this-mode 1)
  :bind ("C-c /" . google-this-mode-submap))

(use-package wgrep
  :ensure t)

;; Silver searcher
(use-package ag
  :ensure t
  :pin melpa-stable
  :config (progn
            (setq ag-highlight-search t)
            (use-package wgrep-ag
              :ensure t)
            )
  :bind (("C-c 5" . ag-regexp)
         ("C-c 6" . ag-project)
         ("C-c 7" . ag-project-at-point)
         ("C-c 8" . ag-regexp-project-at-point)
         ))

;; Ack
(use-package ack-and-a-half
  :ensure t
  :pin melpa-stable
  :config (use-package wgrep-ack
            :ensure t)
  :bind
  (("C-c 1" . ack-and-a-half)
   ("C-c 2" . ack-and-a-half-same)
   ("C-c 3" . ack-and-a-half-find-file)
   ("C-c 4" . ack-and-a-half-find-file-same)))

;; rgrep
(define-key global-map "\C-x\C-r" 'rgrep)

;; anzu
;; Show number of matches while searching
(use-package anzu
  :ensure t
  :pin melpa-stable
  :init
  (global-anzu-mode t))

(provide 'init-search)
