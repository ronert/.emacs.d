(require 'magit)

;; Git Messenger
(require-package 'git-messenger)
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

(global-set-key (kbd "C-=") 'git-gutter:toggle)
(global-set-key (kbd "C-M-=") 'git-gutter:popup-diff)


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

(global-set-key [f16] 'magit-status)
(global-set-key [f17] 'magit-init)

;; (require-package 'magithub)

(require-package 'gist)
(setq gist-view-gist t)

(global-set-key (kbd "C-x g") 'magit-status)
(define-key global-map "\M-\C-g" 'magit-status)

(require-package 'fullframe)
(after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(require-package 'diff-hl)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)

;; git-timemachine
(require-package 'git-timemachine)

(provide 'init-git)
