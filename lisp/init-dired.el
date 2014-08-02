(require-package 'dired+)
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

(require 'stripe-buffer)
(add-hook 'org-mode-hook 'org-table-stripes-enable)
(add-hook 'dired-mode-hook 'stripe-listify-buffer)

;; enable direx
(require 'popwin)
(popwin-mode 1)

(require 'direx)
                                        ;(global-set-key (kbd "C-c C-j") 'direx-project:jump-to-project-root-other-window)

(push '(direx:direx-mode :position left :width 50 :dedicated t)
      popwin:special-display-config)
(global-set-key (kbd "C-c C-j") 'direx-project:jump-to-project-root-other-window)

(provide 'init-dired)
