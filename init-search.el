;; Dash
(require-package 'dash)
(global-set-key (kbd "C-c d") 'dash-at-point)

;; Google this
(require-package 'google-this)
(google-this-mode 1)
(global-set-key (kbd "C-c /") 'google-this-mode-submap)

;; Silver searcher
(require-package 'ag)
(setq ag-highlight-search t)
(global-set-key (kbd "C-c 5") 'ag-regexp)
(global-set-key (kbd "C-c 6") 'ag-project)

(global-set-key (kbd "C-c 7") 'ag-project-at-point)
(global-set-key (kbd "C-c 8") 'ag-regexp-project-at-point)

(global-set-key (kbd "C-c 1") 'ack-and-a-half)
(global-set-key (kbd "C-c 2") 'ack-and-a-half-same)
(global-set-key (kbd "C-c 3") 'ack-and-a-half-find-file)
(global-set-key (kbd "C-c 4") 'ack-and-a-half-find-file-same)

(define-key global-map "\C-x\C-r" 'rgrep)


(provide 'init-search)
