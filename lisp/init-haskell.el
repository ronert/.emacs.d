(use-package haskell-mode
  :ensure t
  :pin melpa-stable
  :config
  (add-to-list 'completion-ignored-extensions ".hi")

  (after-load 'haskell-mode
    (define-key haskell-mode-map (kbd "C-?") 'hoogle))

  (add-hook 'haskell-mode-hook 'run-coding-hook)

  ;; identation setup
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

  (when (executable-find "ghci-ng")
    (setq-default haskell-process-args-cabal-repl
                  e '("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng")))

;;; Flycheck specifics
  (when (> emacs-major-version 23)
    (use-package flycheck-hdevtools
      :ensure t
      :pin melpa-stable)
    (use-package flycheck-haskell
      :ensure t
      :pin melpa-stable
      :config
      (progn
        (after-load 'flycheck
          (add-hook 'haskell-mode-hook #'flycheck-haskell-setup)

          (defun sanityinc/flycheck-haskell-reconfigure ()
            "Reconfigure flycheck haskell settings, e.g. after changing cabal file."
            (interactive)
            (unless (eq major-mode 'haskell-mode)
              (error "Expected to be in haskell-mode"))
            (flycheck-haskell-clear-config-cache)
            (flycheck-haskell-configure)
            (flycheck-mode -1)
            (flycheck-mode))
          )
        ))
    )

  (progn
    (when (> emacs-major-version 23)
      (use-package hayoo
        :ensure t))

    (use-package helm-hayoo
              :ensure t
              :config (define-key haskell-mode-map (kbd "C-*") 'helm-hayoo))

    (dolist (hook '(haskell-mode-hook inferior-haskell-mode-hook haskell-interactive-mode-hook))
      (add-hook hook 'turn-on-haskell-doc-mode)
      (add-hook hook (lambda () (subword-mode +1))))
    (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

    (add-hook 'haskell-interactive-mode-hook 'sanityinc/no-trailing-whitespace)

    (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
    ;; (after-load 'haskell-process
    ;;   (diminish 'interactive-haskell-mode " IntHS"))

    (add-auto-mode 'haskell-mode "\\.ghci\\'")

    (custom-set-variables
     '(haskell-process-suggest-remove-import-lines t)
     '(haskell-process-type 'ghci)
     '(haskell-process-auto-import-loaded-modules t)
     '(haskell-process-log t))

    (use-package hi2
      :ensure t
      :pin melpa-stable
      :config (add-hook 'haskell-mode-hook 'turn-on-hi2))

    (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

    (setq-default haskell-stylish-on-save t)

    (after-load 'haskell-mode
      (define-key haskell-mode-map (kbd "C-c b") 'haskell-interactive-bring)
      (define-key haskell-mode-map (kbd "C-o") 'open-line))
    (when (eval-when-compile (>= emacs-major-version 24))
      (use-package ghci-completion
        :ensure t
        :config
        (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion))
      )

    (eval-after-load 'page-break-lines
      '(push 'haskell-mode page-break-lines-modes))

    ;; Make compilation-mode understand "at blah.hs:11:34-50" lines output by GHC
    (after-load 'compile
      (let ((alias 'ghc-at-regexp))
        (add-to-list
         'compilation-error-regexp-alist-alist
         (list alias " at \\(.*\\.\\(?:l?[gh]hs\\|hi\\)\\):\\([0-9]+\\):\\([0-9]+\\)-[0-9]+$" 1 2 3 0 1))
        (add-to-list
         'compilation-error-regexp-alist alias)))

    ;; Move nested block
    (eval-after-load "haskell-mode"
      '(progn
         (define-key haskell-mode-map (kbd "M-<left>") 'haskell-move-nested-left)
         (define-key haskell-mode-map (kbd "M-<right>") 'haskell-move-nested-right)))

    ;; ghc-mod http://www.mew.org/~kazu/proj/ghc-mod/en/preparation.html
    (autoload 'ghc-init "ghc" nil t)
    (autoload 'ghc-debug "ghc" nil t)
    (add-hook 'haskell-mode-hook (lambda () (ghc-init)))

    ;; Jump to tag
    (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)
    ))


(provide 'init-haskell)
