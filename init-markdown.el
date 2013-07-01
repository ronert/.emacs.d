;; Markdown
(require-package 'markdown-mode)

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.markdown$" . markdown-mode)
            (cons '("\\.md$" . markdown-mode) auto-mode-alist)))

(provide 'markdown)
;;; markdown.el ends here

(provide 'init-markdown)
