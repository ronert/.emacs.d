;; Markdown
(use-package markdown-mode
  :ensure t
  :pin melpa-stable
  :config
  (progn
    (autoload 'markdown-mode "markdown-mode.el"
      "Major mode for editing Markdown files" t)
    (setq auto-mode-alist
          (cons '("\\.markdown$" . markdown-mode)
                (cons '("\\.md$" . markdown-mode) auto-mode-alist)))
    ;; This function will open Marked.app and monitor the current markdown document
    ;; for anything changes.  In other words, it will live reload and convert the
    ;; markdown documment
    (defun markdown-preview-file ()
      "run Marked on the current file and revert the buffer"
      (interactive)
      (shell-command
       (format "open -a /Applications/Marked\\ 2.app %s"
               (shell-quote-argument (buffer-file-name))))
      )
    (global-set-key (kbd "C-c C-o") 'markdown-preview-file)
    ))


;;; markdown.el ends here

(provide 'init-markdown)
