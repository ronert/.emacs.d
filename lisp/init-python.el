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
    )
  )

;; elpy
(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (progn
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

(when (maybe-require-package 'anaconda-mode)
  (after-load 'python
    ;; Anaconda doesn't work on remote servers without some work, so
    ;; by default we enable it only when working locally.
    (add-hook 'python-mode-hook
              (lambda () (unless (file-remote-p default-directory)
                           (anaconda-mode 1))))
    (add-hook 'anaconda-mode-hook 'anaconda-eldoc-mode))
  (after-load 'anaconda-mode
    (define-key anaconda-mode-map (kbd "M-?") nil))
  (when (maybe-require-package 'company-anaconda)
    (after-load 'company
      (after-load 'python
        (push 'company-anaconda company-backends)))))

(when (maybe-require-package 'toml-mode)
  (add-to-list 'auto-mode-alist '("poetry\\.lock\\'" . toml-mode)))


(provide 'init-python)
