;; guide-key

(require-package 'guide-key)

(setq guide-key/guide-key-sequence '("C-x r" "C-x v" "C-c p" "C-x 4" "C-h" "C-y" "C-x 5" "C-c" "C-x" "C-x n"))

(guide-key-mode 1)
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)
(setq guide-key/idle-delay 0.3)

(defun guide-key/my-hook-function-for-org-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "C-c C-x")
  (guide-key/add-local-highlight-command-regexp "org-"))
(add-hook 'org-mode-hook 'guide-key/my-hook-function-for-org-mode)

(defun guide-key/my-hook-function-for-scala-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-highlight-command-regexp "scala-"))
(add-hook 'scala-mode-hook 'guide-key/my-hook-function-for-scala-mode)

(defun guide-key/my-hook-function-for-haskell-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-highlight-command-regexp "haskell-"))
(add-hook 'haskell-mode-hook 'guide-key/my-hook-function-for-haskell-mode)

(defun guide-key/my-hook-function-for-ess-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-guide-key-sequence "M-#")
  (guide-key/add-local-highlight-command-regexp "ess-"))
(add-hook 'ess-mode-hook 'guide-key/my-hook-function-for-ess-mode)

(defun guide-key/my-hook-function-for-python-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-highlight-command-regexp "python-"))
(add-hook 'python-mode-hook 'guide-key/my-hook-function-for-python-mode)

(defun guide-key/my-hook-function-for-ein ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-highlight-command-regexp "ein"))
(add-hook 'ein:notebook-multilang-mode-hook 'guide-key/my-hook-function-for-ein)

(defun guide-key/my-hook-function-for-sql-mode ()
  (guide-key/add-local-guide-key-sequence "C-c")
  (guide-key/add-local-highlight-command-regexp "sql-"))
(add-hook 'sql-mode-hook 'guide-key/my-hook-function-for-sql-mode)

(provide 'init-guidekey)
