(require-package 'exec-path-from-shell)

(setenv "PATH" (concat "/usr/texbin:/usr/local/bin:" (getenv "PATH")))
(setenv "PATH" (concat "~/anaconda/bin:" (getenv "PATH")))

(setq-default ispell-program-name "/usr/local/bin/aspell")

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(provide 'init-exec-path)
