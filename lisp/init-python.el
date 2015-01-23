(use-package python
  :init
  (progn
    (add-hook 'python-mode-hook 'run-coding-hook)
    (use-package anaconda-mode
      :ensure t
      :init (add-hook 'python-mode-hook 'anaconda-mode))
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
    ))

;; elpy
(use-package elpy
  :ensure t
  :pin elpy
  :init
  (progn
    (elpy-enable)
    (elpy-use-ipython))
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

(provide 'init-python)
