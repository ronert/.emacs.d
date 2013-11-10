(require-package 'scala-mode2)

(add-hook 'scala-mode-hook 'run-prog-mode-hook)

(setq scala-indent:align-forms t)
(setq scala-indent:align-parameters t)

;; ensime
(require-package 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(provide 'init-scala)
