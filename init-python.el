
(add-to-list 'load-path "/Users/ronert/Dropbox/.emacs.d/src/python")
(require 'python)
(require-package 'elpy)

(elpy-enable)
(elpy-use-ipython)

(require-package 'websocket)
(require-package 'ein)
;;(setq ein:use-auto-complete t)
;; Or, to enable "superpack" (a little bit hacky improvements):
(setq ein:use-auto-complete-superpack t)

;;  (setq ein:use-smartrep t)
(setq ein:connect-default-notebook "8888/ipythonNotebook")
(add-hook 'ein:connect-mode-hook 'ein:jedi-setup)
(setq ein:use-auto-complete t)
(add-hook 'python-mode-hook 'ein:connect-to-default-notebook)
;;  (setq ein:notebook-modes '(ein:notebook-python-mode))

;;  (setq jedi:setup-keys t)
;;  (add-hook 'python-mode-hook 'jedi:setup)

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

(provide 'init-python)
