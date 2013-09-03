(require-package 'yasnippet)
(setq yas-snippet-dirs
      '("/Users/ronert/Dropbox/dotfiles/.emacs.d/snippets"            ;; personal snippets
        "/Users/ronert/Dropbox/dotfiles/.emacs.d/snippets/shnippets"  ;; haskell snippet repo
        ))

(setq yas-root-directory "/Users/ronert/Dropbox/dotfiles/.emacs.d/snippets")


(yas-global-mode 1)
(global-set-key (kbd "C--") 'yas-expand)

(setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))

(provide 'init-yasnippet)
