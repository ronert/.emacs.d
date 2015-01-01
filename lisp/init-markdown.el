;; Markdown
(use-package markdown-mode
  :ensure t
  :pin melpa-stable
  :init
  (progn
    (autoload 'markdown-mode "markdown-mode.el"
      "Major mode for editing Markdown files" t)
    (setq auto-mode-alist
          (cons '("\\.markdown$" . markdown-mode)
                (cons '("\\.md$" . markdown-mode) auto-mode-alist)))

    ))


;;; markdown.el ends here

(provide 'init-markdown)
