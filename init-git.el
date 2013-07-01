(require-package 'magit)

;; Git Messenger
(require-package 'git-messenger)
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

(global-set-key (kbd "<f18>") 'git-gutter:toggle)
(global-set-key (kbd "<f19>") 'git-gutter:popup-diff)

;; full screen magit-status

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))


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

(add-hook 'magit-mode-hook 'turn-on-magit-push-remote)

;; (require-package 'magithub)

(require-package 'gist)
(setq gist-view-gist t)

(global-set-key (kbd "C-x g") 'magit-status)
(define-key global-map "\M-\C-g" 'magit-status)

(provide 'init-git)
