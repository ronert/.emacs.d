;; scala-mode2
(require-package 'scala-mode2)

(add-hook 'scala-mode-hook 'run-prog-mode-hook)

(setq scala-indent:align-forms t)
(setq scala-indent:align-parameters t)

;; ensime
(require-package 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; sbt-mode
(require-package 'sbt-mode)

(provide 'init-scala)
