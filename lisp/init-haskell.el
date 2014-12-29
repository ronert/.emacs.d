(require-package 'haskell-mode)
(when (> emacs-major-version 23)
  (require-package 'hayoo))

(add-to-list 'completion-ignored-extensions ".hi")

(after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c h") 'hoogle))

(add-hook 'haskell-mode-hook 'run-coding-hook)

;; identation setup
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;; Flycheck specifics
(when (> emacs-major-version 23)
;;  (require-package 'flycheck-hdevtools)
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

;;    (require 'flycheck-hdevtools)
))


(dolist (hook '(haskell-mode-hook inferior-haskell-mode-hook haskell-interactive-mode-hook))
  (add-hook hook 'turn-on-haskell-doc-mode)
  (add-hook hook (lambda () (subword-mode +1))))
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(add-hook 'haskell-interactive-mode-hook 'sanityinc/no-trailing-whitespace)

;; (after-load 'haskell-process
;;   (diminish 'interactive-haskell-mode " IntHS"))

(add-auto-mode 'haskell-mode "\\.ghci\\'")

(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type 'ghci)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t))

;;(require-package 'hi2)
;;(add-hook 'haskell-mode-hook 'turn-on-hi2)

;;(add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template)

(setq-default haskell-stylish-on-save t)

(after-load 'haskell-mode
  (define-key haskell-mode-map (kbd "C-c h") 'hoogle)
  (define-key haskell-mode-map (kbd "C-c b") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-o") 'open-line))

(when (eval-when-compile (>= emacs-major-version 24))
  (require-package 'ghci-completion)
  (add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion))

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

(provide 'init-haskell)
