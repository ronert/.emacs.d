;; Workgroups
(require 'workgroups)
(global-unset-key (kbd "C-y"))
(setq wg-prefix-key (kbd "C-y"))
(workgroups-mode 1)
(wg-load "/Users/ronert/Dropbox/dotfiles/.emacs.d/workgorups")

;; save a list of open files in ~/.emacs.d/.emacs.desktop
;; save the desktop file automatically if it already exists
(setq desktop-path '("~/.emacs.d"))
(setq desktop-save 'if-exists)
(desktop-save-mode 1)
(defadvice desktop-read (around trace-desktop-errors)
  (let ((debug-on-error t))
    ad-do-it))
(desktop-save-mode 1)

;;----------------------------------------------------------------------------;; Restore histories and registers after saving
;;----------------------------------------------------------------------------
(require-package 'session)

(setq session-save-file (expand-file-name "/Users/ronert/Dropbox/dotfiles/.emacs.d/.session"))
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

;; Save point position between sessions
(require-package 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(provide 'init-sessions)
