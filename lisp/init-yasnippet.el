(use-package yasnippet
             :ensure t
             :mode ("\\.yasnippet$" . snippet-mode)
             :config
             (progn
               (setq yas-snippet-dirs
                     '("/Users/ronert/Dropbox/dotfiles/.emacs.d/snippets"
                       "/Users/ronert/Dropbox/dotfiles/.emacs.d/snippets/shnippet"))
               (define-key yas-keymap (kbd "<return>") 'yas-exit-all-snippets)

               ;; Inter-field navigation
               (defun yas-goto-end-of-active-field ()
                 (interactive)
                 (let* ((snippet (car (yas--snippets-at-point)))
                        (position (yas--field-end (yas--snippet-active-field snippet))))
                   (if (= (point) position)
                       (move-end-of-line 1)
                     (goto-char position))))
               (defun yas-goto-start-of-active-field ()
                 (interactive)
                 (let* ((snippet (car (yas--snippets-at-point)))
                        (position (yas--field-start (yas--snippet-active-field snippet))))
                   (if (= (point) position)
                       (move-beginning-of-line 1)
                     (goto-char position))))
               (define-key yas-keymap (kbd "C-e") 'yas-goto-end-of-active-field)
               (define-key yas-keymap (kbd "C-a") 'yas-goto-start-of-active-field)

               ;; No dropdowns please, yas
               (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))

               ;; No need to be so verbose
               (setq yas-verbosity 1)

               ;; Wrap around region
               (setq yas-wrap-around-region t)

               ;; Unbind yas from tab
               (define-key yas-minor-mode-map [(tab)] nil)
               (define-key yas-minor-mode-map (kbd "TAB") nil)
               (define-key yas-minor-mode-map (kbd "C--") #'yas-expand)
               (yas-global-mode 1)
               ))


(provide 'init-yasnippet)
