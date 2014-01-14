(defun coding-settings ()
  (font-lock-comment-annotations)
  (highlight-symbol-mode +1)
  (highlight-symbol-nav-mode +1)
  (smartparens-mode +1)
  (flycheck-mode +1)
  (prelude-add-watchwords)
  )


(defun coding-keys ()
  (local-set-key (kbd "C-c C-n") 'flycheck-tip-cycle)
  )


(add-hook 'coding-hook 'coding-settings)
(add-hook 'coding-hook 'coding-keys)


(defun run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'coding-hook))


(provide 'init-coding)
