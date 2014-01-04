(require-package 'pig-mode)
(require-package 'hive)

;; (setq pig-executable "/path/to/pig-0.11.1/bin/pig")
;; (setq pig-executable-options '("-x" "local"))
;; (setq pig-executable-prompt-regexp "^grunt> ")
;; (setq pig-indent-level 4)
;; (setq pig-version "0.11.1")

(autoload 'pig-snippets-initialize "pig-snippets" nil nil nil)
(eval-after-load 'yasnippet '(pig-snippets-initialize))

(provide 'init-hadoop)
;;; hadoop.el ends here
