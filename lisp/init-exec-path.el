(use-package exec-path-from-shell
  :ensure t
  :pin melpa-stable
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
  :config
  (progn
    (setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))
    (setenv "PATH" (concat "~/anaconda/bin:" (getenv "PATH")))
    (setq-default ispell-program-name "/usr/local/bin/aspell"))
  )

(provide 'init-exec-path)
