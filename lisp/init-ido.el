;; ido
(use-package ido
  :ensure t
  :config
  (progn
    (use-package ido-sort-mtime
      :ensure t
      :config (ido-sort-mtime-mode 1))
    ;; filecache
    (use-package filecache
      :ensure t
      :config
      (progn
        (defun file-cache-ido-find-file (file)
          (interactive (list (file-cache-ido-read "File: "
                                                  (mapcar
                                                   (lambda (x)
                                                     (car x))
                                                   file-cache-alist))))
          (let* ((record (assoc file file-cache-alist)))
            (find-file
             (expand-file-name
              file
              (if (= (length record) 2)
                  (car (cdr record))
                (file-cache-ido-read
                 (format "Find %s in dir: " file) (cdr record)))))))

        (defun file-cache-ido-read (prompt choices)
          (let ((ido-make-buffer-list-hook
                 (lambda ()
                   (setq ido-temp-list choices))))
            (ido-read-buffer prompt))))
      :bind (("C-c f" . file-cache-ido-find-file)
             ("C-c a" . file-cache-add-directory-using-find)))
    ))


(use-package imenu)

(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 10)


;; Use ido everywhere
(setq max-lisp-eval-depth 12000)
(require-package 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read webjump 'webjump)
(ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)

;; enable flx
;; (require 'flx-ido)
;; (flx-ido-mode 1)
;; ;; disable ido faces to see flx highlights.
;; (setq ido-use-faces nil)
;; ;; increase garbage collection threshold
;; (setq gc-cons-threshold 20000000)

;; use ido vertical mode
(use-package ido-vertical-mode
  :ensure t
  :pin melpa-stable
  :config (ido-vertical-mode t))

;; up down arrows to navigate
;;(define-key ido-completion-map (kbd "<down>") 'ido-next-match)
;;(define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
  (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

(provide 'init-ido)
