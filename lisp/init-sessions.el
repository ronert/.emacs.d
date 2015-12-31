;; save a list of open files in ~/.emacs.d/.emacs.desktop
;; save the desktop file automatically if it already exists
(use-package desktop
  :ensure t
  :config (progn
            (desktop-save-mode 1)
            (defun my-desktop-save ()
              (interactive)
              ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
              (if (eq (desktop-owner) (emacs-pid))
                  (desktop-save desktop-dirname)))
            (add-hook 'auto-save-hook 'my-desktop-save)

            (run-at-time 3600 3600 'my-desktop-save)

            (setq desktop-path '("~/"))
            (setq desktop-save 'if-exists)
            (desktop-save-mode 1)
            (defadvice desktop-read (around trace-desktop-errors)
              (let ((debug-on-error t))
                ad-do-it))
            (desktop-save-mode 1)
            )
  )


;;----------------------------------------------------------------------------;; Restore histories and registers after saving
;;----------------------------------------------------------------------------
(use-package session
  :ensure t
  :pin melpa-stable
  :config
  (progn
    (setq session-save-file (expand-file-name "~/.session"))
    (add-hook 'after-init-hook 'session-initialize)

    ;; save a bunch of variables to the desktop file
    ;; for lists specify the len of the maximal saved data also
    (setq desktop-globals-to-save
          (append '((extended-command-history . 30)
                    (file-name-history        . 100)
                    (ido-last-directory-list  . 100)
                    (ido-work-directory-list  . 100)
                    (ido-work-file-list       . 100)
                    (grep-history             . 30)
                    (compile-history          . 30)
                    (minibuffer-history       . 50)
                    (query-replace-history    . 60)
                    (read-expression-history  . 60)
                    (regexp-history           . 60)
                    (regexp-search-ring       . 20)
                    (search-ring              . 20)
                    (comint-input-ring        . 50)
                    (shell-command-history    . 50)
                    desktop-missing-file-warning
                    tags-file-name
                    register-alist)))
    ))


;; Save point position between sessions
(use-package saveplace
  :ensure t
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name "~/.places"))
  )

;; Workgroups
(use-package workgroups2
  :ensure t
  :config (progn
            (global-unset-key (kbd "C-y"))
            (setq wg-prefix-key (kbd "C-y"))
            (setq wg-session-file "~/.emacs.d/.emacs_workgroups")
            (workgroups-mode 1)
            ;; What to do on Emacs exit / workgroups-mode exit?
            (setq wg-emacs-exit-save-behavior           'save)      ; Options: 'save 'ask nil
            (setq wg-workgroups-mode-exit-save-behavior 'save)      ; Options: 'save 'ask nil

            ;; Mode Line changes
            ;; Display workgroups in Mode Line?
            (setq wg-mode-line-display-on t)          ; Default: (not (featurep 'powerline))
            (setq wg-flag-modified t)                 ; Display modified flags as well
            (setq wg-mode-line-decor-left-brace "["
                  wg-mode-line-decor-right-brace "]"  ; how to surround it
                  wg-mode-line-decor-divider ":")
            )
  )

(provide 'init-sessions)
