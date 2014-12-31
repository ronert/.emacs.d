(use-package csv-mode
  :ensure t)
(use-package csv-nav
  :ensure t
  :init
  (autoload 'csv-nav-mode "csv-nav" "Major mode for navigating comma-separated value files." t)
  :config
  (setq csv-separators '("," ";" "|" " ")))

(provide 'init-csv)
