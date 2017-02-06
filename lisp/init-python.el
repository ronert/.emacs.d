(use-package python
  :config
  (progn
    (add-hook 'python-mode-hook 'run-coding-hook)
    (add-hook 'python-mode-hook 'my-python-customizations)
    (defun my-python-customizations ()
      "set up my personal customizations for python mode"
      ;; put other customizations in here
      (define-key python-mode-map (kbd "C-c d") 'add-docstring))
    (defun add-docstring (&optional arg)
      "Keyboard macro."
      (interactive "p")
      (kmacro-exec-ring-item `(,(kbd "C-r def C-n C-a C-m C-p C-i C-u 6 \" C-u 3 C-b") 0 "%d")
                             arg)
      )
    (setq python-shell-exec-path "~/anaconda3/bin/")
    (setq python-shell-interpreter "ipython")
    ))

elpy
(use-package elpy
  :ensure t
  :pin elpy
  :config
  (progn
    (elpy-enable)
    (elpy-use-ipython)
    (setq-default flycheck-flake8-maximum-line-length 160)
    (setq elpy-rpc-python-command "~/anaconda3/bin/python")
    )
  )

;; ein
(use-package ewoc)
(use-package websocket
  :ensure t)
(use-package ein
  :ensure t
  :config
  (progn
    (setq ein:connect-default-notebook "8888/ipythonNotebook")
    (add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
    )
  )

;; pip-requirements
(use-package pip-requirements
  :ensure t
  :pin melpa-stable)

;; sort imports
(use-package pyimpsort
  :ensure t)

(use-package live-py-mode
  :ensure t
  :config)

(use-package pytest
  :ensure t)

(provide 'init-python)
