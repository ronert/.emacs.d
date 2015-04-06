(defun font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations. This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))

(use-package highlight-symbol
  :ensure t)

(defun coding-settings ()
  (font-lock-comment-annotations)
  (highlight-symbol-mode +1)
  (highlight-symbol-nav-mode +1)
  (smartparens-mode +1)
  (flycheck-mode +1)
  (which-function-mode +1)
  )


(defun coding-keys ()
  (local-set-key (kbd "C-c C-n") 'flycheck-tip-cycle)
  (local-set-key (kbd "C-c d") 'dash-at-point)
  )

(add-hook 'coding-hook 'coding-settings)
(add-hook 'coding-hook 'coding-keys)

(use-package diff-hl
  :ensure t
  :config (add-hook 'coding-hook 'turn-on-diff-hl-mode)
  )

(defun run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'coding-hook))


(provide 'init-coding)
