(require-package 'csv-mode)
(require-package 'csv-nav)

(autoload 'csv-nav-mode "csv-nav" "Major mode for navigating comma-separated value files." t)

(setq csv-separators '("," ";" "|" " "))

(provide 'init-csv)
