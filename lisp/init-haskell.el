(require-package 'haskell-mode)
(add-to-list 'auto-mode-alist '("\\.ghci\\'" . haskell-mode))
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode);
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c h") 'hoogle))

(add-hook 'haskell-mode-hook 'run-coding-hook)

;; Make compilation-mode understand "at blah.hs:11:34-50" lines output by GHC
(after-load 'compile
  (let ((alias 'ghc-at-regexp))
    (add-to-list
     'compilation-error-regexp-alist-alist
     (list alias " at \\(.*\\.\\(?:l?[gh]hs\\|hi\\)\\):\\([0-9]+\\):\\([0-9]+\\)-[0-9]+$" 1 2 3 0 1))
    (add-to-list
     'compilation-error-regexp-alist alias)))

(require 'ghci-completion)
(add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)

(require 'flymake-haskell-multi)
(add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)
(add-hook 'haskell-mode-hook 'highlight-indentation-mode)


(when (> emacs-major-version 23)
  (require-package 'flycheck-hdevtools)
  (require-package 'flycheck-haskell)
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

    (defadvice haskell-mode-stylish-buffer (around skip-if-flycheck-errors activate)
      "Don't run stylish-buffer if the buffer appears to have a syntax error.
This isn't a hard guarantee, since flycheck might sometimes not run until the file has
been saved."
      (unless (flycheck-has-current-errors-p 'error)
        ad-do-it))

    (require 'flycheck-hdevtools)))

(defun pretty-lambdas-haskell ()
  (font-lock-add-keywords
   nil `((,(concat "(?\\(" (regexp-quote "\\") "\\)")
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(when (window-system)
  (add-hook 'haskell-mode-hook 'pretty-lambdas-haskell))

(setq-default haskell-stylish-on-save t)

(dolist (hook '(haskell-mode-hook inferior-haskell-mode-hook haskell-interactive-mode-hook)))
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-interactive-mode-hook 'sanityinc/no-trailing-whitespace)
(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

(provide 'init-haskell)
;;; init-haskell.el ends here
