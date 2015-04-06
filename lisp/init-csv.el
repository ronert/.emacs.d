(use-package csv-mode
  :ensure t)
(use-package csv-nav
  :ensure t
  :config
  (autoload 'csv-nav-mode "csv-nav" "Major mode for navigating comma-separated value files." t)
  :config
  (setq csv-separators '("," ";" "|" " ")))

(provide 'init-csv)
