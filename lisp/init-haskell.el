(require 'haskell-mode)
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
         (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)

         (defadvice haskell-mode-stylish-buffer (around skip-if-flycheck-errors activate)
                  "Don't run stylish-buffer if the buffer appears to have a syntax error."
                  (unless (flycheck-has-current-errors-p 'error)
                             ad-do-it))))

;; (defun sanityinc/haskell-enable-flymake ()
;;   (if (package-installed-p 'ghc)
;;       (progn
;;         (ghc-init)
;;         (flymake-mode))
;;     (flymake-haskell-multi-load)))
;; (add-hook 'haskell-mode-hook 'sanityinc/haskell-enable-flymake)

;; (require 'scion)

;; ;; if ./cabal/bin is not in your $PATH
;; (setq scion-program "~/.cabal/bin/scion-server")

;; (defun my-haskell-hook ()
;;   ;; Whenever we open a file in Haskell mode, also activate Scion
;;   (scion-mode 1)
;;   ;; Whenever a file is saved, immediately type check it and
;;   ;; highlight errors/warnings in the source.
;;   (scion-flycheck-on-save 1))

;; (add-hook 'haskell-mode-hook 'my-haskell-hook)

;; ;; Use ido-mode completion (matches anywhere, not just beginning)
;; ;;
;; ;; WARNING: This causes some versions of Emacs to fail so badly
;; ;; that Emacs needs to be restarted.
;; (setq scion-completing-read-function 'ido-completing-read)

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

(provide 'init-haskell)
;;; init-haskell.el ends here
