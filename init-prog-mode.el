(add-hook 'prog-mode-hook 'font-lock-comment-annotations)

(defun run-prog-mode-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'prog-mode-hook))

(require-package 'highlight-symbol)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)
(add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)

(provide 'init-prog-mode)
