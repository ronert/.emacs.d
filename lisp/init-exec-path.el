(use-package exec-path-from-shell
  :ensure t
  :pin melpa-stable
  :config
  (progn
    (when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize))

    (progn
      (setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))
      (setenv "PATH" (concat "~/.local/bin:" (getenv "PATH")))
      (setenv "PATH" (concat "~/anaconda3/bin:" (getenv "PATH")))
      (setenv "PYTHONPATH" (concat "~/anaconda3/bin:" (getenv "PYTHONPATH")))
      (setq-default ispell-program-name "/usr/local/bin/aspell"))

    (add-to-list 'exec-path "/usr/texbin:/usr/local/bin")
    (add-to-list 'exec-path "~/anaconda3/bin")
    (add-to-list 'exec-path "~/.local/bin")
    )
  )

(provide 'init-exec-path)
