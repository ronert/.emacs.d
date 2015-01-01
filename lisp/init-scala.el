;; scala-mode2
(use-package scala-mode2
  :ensure t
  :pin melpa-stable
  :config
  (progn
    (add-hook 'scala-mode-hook 'run-coding-hook)
    (setq scala-indent:align-forms t)
    (setq scala-indent:align-parameters t)

    ;; ensime
    (use-package ensime
      :ensure t
      :init (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

    ;; sbt-mode
    (use-package sbt-mode
      :ensure t
      :init
      (progn
        (add-hook 'scala-mode-hook '(lambda ()
                                      ;; sbt-find-definitions is a command that tries to find (with grep)
                                      ;; the definition of the thing at point.
                                      (local-set-key (kbd "M-.") 'sbt-find-definitions)

                                      ;; use sbt-run-previous-command to re-compile your code after changes
                                      (local-set-key (kbd "C-x '") 'sbt-run-previous-command)
                                      ))))
    )
  )


(provide 'init-scala)
