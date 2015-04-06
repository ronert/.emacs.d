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
   ("C-M-=" . git-gutter:popup-diff))
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

(provide 'init-git)
