(use-package dired+
  :ensure t
  :config
  (use-package stripe-buffer
    :ensure t
    :pin melpa-stable
    :init
    (progn
      (add-hook 'org-mode-hook 'org-table-stripes-enable)
      (add-hook 'dired-mode-hook 'stripe-listify-buffer))
    )
)

;; Also auto refresh dired
(setq global-auto-revert-non-file-buffers t)

;; Always reload dired after creating a directory
(defadvice dired-create-directory (after revert-buffer-after-create)
  (revert-buffer))
(ad-activate 'dired-create-directory)

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

(global-set-key (kbd "<f12>") 'wdired-change-to-wdired-mode)

;; find-name-dired
(global-set-key (kbd "C-c s") 'find-name-dired)
;; Dired jump
(global-set-key (kbd "C-x C-j") 'dired-jump)

(load "dired-x")

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "F" 'my-dired-find-file)
     (defun my-dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))


;; enable direx
(use-package popwin
  :ensure t
  :pin melpa-stable
  :config
  (popwin-mode 1))

(use-package project-explorer
  :ensure t
  :pin melpa-stable
  :bind
  ("C-c C-j" . project-explorer-open)
  ("C-c j" . project-explorer-helm)
  )

(provide 'init-dired)
