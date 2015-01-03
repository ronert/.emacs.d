(use-package helm
  :ensure t
  :pin melpa-stable
  :bind (("M-x" . helm-M-x)
         ("C-c h" . helm-mini)
         ("M-z" . helm-show-kill-ring)
         ("C-x f" . helm-find-files))
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
                  helm-apropos-function-list            t)
            (require 'helm-config)
            (helm-mode 1)
            (semantic-mode 1)
            ;; activate helm-descbinds
            (use-package helm-descbinds
              :ensure t
              :init (helm-descbinds-mode))
            ;; activate helm-R
            (use-package helm-R
              :ensure t)
            ;; activate helm-R
            (use-package helm-ack
              :ensure t)
            (use-package helm-themes
              :ensure t
              :pin melpa-stable)
            (use-package helm-c-yasnippet
              :ensure t
              :config
              (setq helm-yas-space-match-any-greedy t)
              :bind ("C-c y" . helm-yas-complete))
            )
  )

(provide 'init-helm)
