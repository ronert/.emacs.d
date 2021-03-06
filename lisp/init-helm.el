(use-package helm
  :ensure t
  :pin melpa-stable
  :bind (("M-x" . helm-M-x)
         ("C-c h" . helm-mini)
         ("M-z" . helm-show-kill-ring)
         ("C-x f" . helm-find-files)
         ("C-c C-i" . helm-semantic-or-imenu))
  :config (progn
            ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
            ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
            ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
            (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
            (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
            (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

            (when (executable-find "curl")
              (setq helm-google-suggest-use-curl-p t))

            (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
                  helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
                  helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
                  helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
                  helm-ff-file-name-history-use-recentf t
                  helm-M-x-fuzzy-match                  t
                  helm-buffers-fuzzy-matching           t
                  helm-recentf-fuzzy-match              t
                  helm-semantic-fuzzy-match             t
                  helm-imenu-fuzzy-match                t
                  helm-apropos-function-list            t
                  helm-exit-idle-delay                  0)
            (require 'helm-config)
            (helm-mode 1)
            ;; (semantic-mode 1)
            ;; activate helm-descbinds
            (use-package helm-descbinds
              :ensure t
              :config (helm-descbinds-mode)
              :bind ("C-c C-h" . helm-descbinds))
            (use-package helm-ag
              :ensure t)
            (use-package helm-ack
              :ensure t)
            (use-package helm-themes
              :ensure t
              :pin melpa-stable)
            (use-package helm-c-yasnippet
              :ensure t
              :config
              (progn
                (setq helm-yas-space-match-any-greedy t)
                (yas-load-directory "/Users/ronert/Dropbox/dotfiles/.emacs.d/snippets"))
              :bind ("C-c y" . helm-yas-complete))
            )
  )

(provide 'init-helm)
