(use-package magit
  :ensure t
  :pin melpa-stable
  :config
  (progn
    (defun magit-toggle-whitespace ()
      (interactive)
      (if (member "-w" magit-diff-options)
          (magit-dont-ignore-whitespace)
        (magit-ignore-whitespace)))

    (defun magit-ignore-whitespace ()
      (interactive)
      (add-to-list 'magit-diff-options "-w")
      (magit-refresh))

    (defun magit-dont-ignore-whitespace ()
      (interactive)
      (setq magit-diff-options (remove "-w" magit-diff-options))
      (magit-refresh))

    (use-package fullframe
      :ensure t
      :config (after-load 'magit
              (fullframe magit-status magit-mode-quit-window)))

    (global-set-key [f16] 'magit-status)
    (global-set-key [f17] 'magit-init)
    (global-set-key (kbd "C-x g") 'magit-status)
    (define-key global-map "\M-\C-g" 'magit-status)

    (setq magit-push-always-verify nil)
    )
  )



;; Git Messenger
(use-package git-messenger
  :ensure t
  :pin melpa-stable
  :bind
  ("C-x v p" . git-messenger:popup-message))

(use-package git-gutter
  :ensure t
  :bind
  (
   ("C-=" . git-gutter:toggle)
   ("C-M-=" . git-gutter:popup-hunk))
  )


(use-package gist
  :ensure t
  :pin melpa-stable
  :config
  (setq gist-view-gist t))

(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)

(use-package git-timemachine
  :ensure t
  :pin melpa-stable)

;; Open PR
(defun endless/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/compare/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-current-remote)
                       "url"))
           (magit-get-current-branch))))

(eval-after-load 'magit
  '(define-key magit-mode-map "V"
     #'endless/visit-pull-request-url))

;; set up ediff
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))
(csetq ediff-window-setup-function 'ediff-setup-windows-plain)
(csetq ediff-split-window-function 'split-window-horizontally)
(defun ora-ediff-hook ()
  (ediff-setup-keymap)
  (define-key ediff-mode-map "j" 'ediff-next-difference)
  (define-key ediff-mode-map "k" 'ediff-previous-difference))

(add-hook 'ediff-mode-hook 'ora-ediff-hook)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(provide 'init-git)
