(require-package 'yasnippet)
(setq yas-snippet-dirs
      '("/Users/ronert/Dropbox/dotfiles/.emacs.d/snippets"            ;; personal snippets
        "/Users/ronert/Dropbox/dotfiles/.emacs.d/snippets/shnippets"  ;; haskell snippet repo
        ))

(setq yas-root-directory "/Users/ronert/Dropbox/dotfiles/.emacs.d/snippets")


(yas-global-mode 1)
(global-set-key (kbd "C--") 'yas-expand)

;;(setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))

;; Jump to end of snippet definition
(define-key yas-keymap (kbd "<return>") 'yas/exit-all-snippets)

;; Inter-field navigation
(defun yas/goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
        (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

(defun yas/goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
        (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

(define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)
(define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)

;; No dropdowns please, yas
(setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))

;; No need to be so verbose
(setq yas-verbosity 1)

;; Wrap around region
(setq yas-wrap-around-region t)

(provide 'init-yasnippet)
