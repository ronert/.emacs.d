;; Google this
(use-package google-this
  :ensure t
  :pin melpa-stable
  :init (google-this-mode 1)
  :bind (kbd "C-c /" . google-this-mode-submap))

;; Silver searcher
(use-package ag
  :ensure t
  :pin melpa-stable
  :config (setq ag-highlight-search t)
  :bind ((global-set-key (kbd "C-c 5") 'ag-regexp)
         (global-set-key (kbd "C-c 6") 'ag-project)
         (global-set-key (kbd "C-c 7") 'ag-project-at-point)
         (global-set-key (kbd "C-c 8") 'ag-regexp-project-at-point)
         ))

;; Ack
(use-package ack-and-a-half
  :ensure t
  :pin melpa-stable
  :bind
  ((global-set-key (kbd "C-c 1") 'ack-and-a-half)
   (global-set-key (kbd "C-c 2") 'ack-and-a-half-same)
   (global-set-key (kbd "C-c 3") 'ack-and-a-half-find-file)
   (global-set-key (kbd "C-c 4") 'ack-and-a-half-find-file-same)))

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
