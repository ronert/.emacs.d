(require 'python)

;; elpy
(require 'elpy)
(elpy-enable)
(elpy-use-ipython)
(local-set-key (kbd "C-c <up>") 'winner-undo)
(add-hook 'elpy-mode-hook
          (lambda ()
            (define-key elpy-mode-map (kbd "C-<left>") 'python-indent-shift-left)
            (define-key elpy-mode-map (kbd "C-<right>") 'python-indent-shift-right)
            )
          )

;; ein
(require-package 'websocket)
(require-package 'ein)
(setq ein:use-auto-complete-superpack t)
(setq ein:connect-default-notebook "8888/ipythonNotebook")
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)

;; python mode
(add-hook 'python-mode-hook 'my-python-customizations)
(defun my-python-customizations ()
  "set up my personal customizations for python mode"
  ;; put other customizations in here
  (define-key python-mode-map (kbd "C-c d") 'add-docstring))
(defun add-docstring (&optional arg)
  "Keyboard macro."
  (interactive "p")
  (kmacro-exec-ring-item `(,(kbd "C-r def C-n C-a C-m C-p C-i C-u 6 \" C-u 3 C-b") 0 "%d")
                         arg))

(add-hook 'python-mode-hook 'run-coding-hook)

(provide 'init-python)
