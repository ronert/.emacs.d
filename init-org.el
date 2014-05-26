;; Set up org-mode
(setq org-replace-disputed-keys t)
(require 'org)

;; Markdown exporter
(require 'ox-md)

(setq org-completion-use-ido t)
;; (require 'org-special-blocks)
;; (if window-system (require 'org-mouse))

;; Compatibility with WindMove
;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(add-hook 'org-mode-hook
          (let ((original-command (lookup-key org-mode-map [tab])))
            `(lambda ()
               (setq yas-fallback-behavior
                     '(apply ,original-command))
               (local-set-key [tab] 'yas-expand))))

(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))


(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key "\M-\C-n" 'outline-next-visible-heading)
            (local-set-key "\M-\C-p" 'outline-previous-visible-heading)
            (local-set-key "\M-\C-u" 'outline-up-heading)
            ;; table
            (local-set-key "\M-\C-w" 'org-table-copy-region)
            (local-set-key "\M-\C-y" 'org-table-paste-rectangle)
            (local-set-key "\M-\C-l" 'org-table-sort-lines)
            ;; display images
            (local-set-key "\M-I" 'org-toggle-iimage-in-org)
            ;; yasnippet (using the new org-cycle hooks)
            ;; (make-variable-buffer-local 'yas/trigger-key)
            ;; (setq yas/trigger-key [tab])
            ;; (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            ;; (define-key yas/keymap [tab] 'yas/next-field)
            ))

(org-babel-lob-ingest
 (expand-file-name
  "library-of-babel.org"
  (expand-file-name
   "babel"
   (expand-file-name
    "contrib"
    (expand-file-name
     "org-mode"
     (expand-file-name "src" dotfiles-dir))))))

(setq org-use-speed-commands t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (R . t)
   (perl . t)
   (ruby . t)
   (python . t)
   (js . t)
   (haskell . t)
   (clojure . t)
   (ditaa . t)))

(add-to-list 'org-src-lang-modes
             '("r" . ess-mode))

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(setq org-confirm-babel-evaluate nil)

(unless (boundp 'Info-directory-list)
  (setq Info-directory-list Info-default-directory-list))
(setq Info-directory-list
      (cons (expand-file-name
             "doc"
             (expand-file-name
              "org"
              (expand-file-name "src" dotfiles-dir)))
            Info-directory-list))


;; for org-mode
(defun org-mode-is-intrusive ()
  ;; Make something work in org-mode:
  ;; (local-unset-key (kbd "something I use"))
  (local-unset-key (kbd "C-y"))
  (local-unset-key (kbd "C-#"))
  (local-unset-key (kbd "C-,"))
  (local-unset-key (kbd "s-<left>"))
  (local-unset-key (kbd "s-<right>"))
  (local-set-key (kbd "C-z") 'org-yank)
  (local-set-key (kbd "C-,") 'hippie-expand)
  )
(add-hook 'org-mode-hook 'org-mode-is-intrusive)

(require 'org-octopress)
(setq org-octopress-directory-top       "~/Dropbox/octopress/source")
(setq org-octopress-directory-posts     "~/Dropbox/octopress/source/_posts")
(setq org-octopress-directory-org-top   "~/Dropbox/octopress/source")
(setq org-octopress-directory-org-posts "~/Dropbox/octopress/source/blog")
(setq org-octopress-setup-file          "~/Dropbox/octopress/setupfile.org")

(setq org-latex-pdf-process '("latexmk -pdflatex='lualatex --shell-escape' -pdf %f"))

(setq org-export-backends (quote (
                                  beamer)))



;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(defun org-linkify (label)
  "Turn a URL into an org-mode link."
  (interactive "sLabel: ")
  (let ((url (thing-at-point 'url))
        (bnds (bounds-of-thing-at-point 'url)))
    (delete-region (car bnds) (cdr bnds))
    (insert (concat "[[" url "][" label "]]"))))

;; cdlatex for editing math
(add-hook 'org-mode-hook 'turn-on-org-cdlatex)


(setq org-startup-indented t)
(global-set-key (kbd "C-c ö") 'org-cdlatex-mode)
(global-set-key (kbd "C-c ü") 'org-edit-src-code)
(global-set-key (kbd "C-c ä") 'org-edit-src-exit)
(add-to-list 'org-src-lang-modes
             '("r" . R))
(setq org-confirm-babel-evaluate nil)
(setq org-src-fontify-natively t)
(setq org-confirm-babel-evaluate nil)
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'org-display-inline-images)

(add-to-list 'org-structure-template-alist
             '("S" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
             '("r" "#+begin_src R\n\n#+end_src" "<src lang=\"r\">\n\n</src>"))

(org-add-link-type
 "cite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "cite:" desc)))
         (format "\\cite{%s}" path)
       (format "\\cite[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))
(org-add-link-type
 "Cite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "Cite:" desc)))
         (format "\\Cite{%s}" path)
       (format "\\Cite[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))
(org-add-link-type
 "parencite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "parencite:" desc)))
         (format "\\parencite{%s}" path)
       (format "\\parencite[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))
(org-add-link-type
 "Parencite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "Parencite:" desc)))
         (format "\\Parencite{%s}" path)
       (format "\\Parencite[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))
(org-add-link-type
 "footcite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "footcite:" desc)))
         (format "\\footcite{%s}" path)
       (format "\\footcite[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))
(org-add-link-type
 "footcitetext" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "footcitetext:" desc)))
         (format "\\footcitetext{%s}" path)
       (format "\\footcitetext[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))

(org-add-link-type
 "cite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "cite:" desc)))
         (format "\\cite{%s}" path)
       (format "\\cite[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))

(org-add-link-type
 "Cite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "Cite:" desc)))
         (format "\\Cite{%s}" path)
       (format "\\Cite[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))
(org-add-link-type
 "parencite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "parencite:" desc)))
         (format "\\parencite{%s}" path)
       (format "\\parencite[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))
(org-add-link-type
 "Parencite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "Parencite:" desc)))
         (format "\\Parencite{%s}" path)
       (format "\\Parencite[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))
(org-add-link-type
 "footcite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "footcite:" desc)))
         (format "\\footcite{%s}" path)
       (format "\\footcite[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))
(org-add-link-type
 "footcitetext" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "footcitetext:" desc)))
         (format "\\footcitetext{%s}" path)
       (format "\\footcitetext[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))

(org-add-link-type
 "textcite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "textcite:" desc)))
         (format "\\textcite{%s}" path)
       (format "\\textcite[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))
(org-add-link-type
 "Textcite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "Textcite:" desc)))
         (format "\\Textcite{%s}" path)
       (format "\\Textcite[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))
(org-add-link-type
 "smartcite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "smartcite:" desc)))
         (format "\\smartcite{%s}" path)
       (format "\\smartcite[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))
(org-add-link-type
 "Smartcite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "Smartcite:" desc)))
         (format "\\Smartcite{%s}" path)
       (format "\\Smartcite[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))
(org-add-link-type
 "cite*" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "cite*:" desc)))
         (format "\\cite*{%s}" path)
       (format "\\cite*[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))
(org-add-link-type
 "parencite*" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "parencite*:" desc)))
         (format "\\parencite*{%s}" path)
       (format "\\parencite*[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))
(org-add-link-type
 "supercite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (format "\\cite*{%s}" path)))))
(org-add-link-type
 "textcite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "textcite:" desc)))
         (format "\\textcite{%s}" path)
       (format "\\textcite[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))
(org-add-link-type
 "Textcite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "Textcite:" desc)))
         (format "\\Textcite{%s}" path)
       (format "\\Textcite[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))
(org-add-link-type
 "smartcite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "smartcite:" desc)))
         (format "\\smartcite{%s}" path)
       (format "\\smartcite[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))
(org-add-link-type
 "Smartcite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "Smartcite:" desc)))
         (format "\\Smartcite{%s}" path)
       (format "\\Smartcite[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))
(org-add-link-type
 "cite*" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "cite*:" desc)))
         (format "\\cite*{%s}" path)
       (format "\\cite*[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))
(org-add-link-type
 "parencite*" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (if (or (not desc) (equal 0 (search "parencite*:" desc)))
         (format "\\parencite*{%s}" path)
       (format "\\parencite*[%s][%s]{%s}"
               (cadr (split-string desc ";"))
               (car (split-string desc ";"))  path))))))
(org-add-link-type
 "supercite" 'ebib
 (lambda (path desc format)
   (cond
    ((eq format 'html)
     (format "(<cite>%s</cite>)" path))
    ((eq format 'latex)
     (format "\\cite*{%s}" path)))))

(global-set-key (kbd "C-c g") 'omlg-grab-link)

(add-hook 'org-mode-hook
          (lambda ()
            (set-face-attribute 'org-level-1 nil :height 1.5)
            (set-face-attribute 'org-level-2 nil :height 1.2)
            (set-face-attribute 'org-level-3 nil :height 1.1)
            (set-face-attribute 'org-level-4 nil :height 1.1)
            (set-face-attribute 'org-level-5 nil :height 1.1)))

(add-hook 'org-mode-hook 'smart-quote-keys)

(setq org-src-window-setup 'current-window)

;; LaTeX settings
;; (require 'org-latex)
;; Choose either listings or minted for exporting source code blocks.
;; Using minted (as here) requires pygments be installed. To use the
;; default listings package instead, use
;; (setq org-latex-listings t)
;; and change references to "minted" below to "listings"
(setq org-latex-listings 'minted)

;; default settings for minted code blocks
(setq org-latex-minted-options
      '(;("frame" "single")
        ("bgcolor" "bg") ; bg will need to be defined in the preamble of your document. It's defined in org-preamble-pdflatex.sty and org-preamble-xelatex.sty below.
        ("fontsize" "\\scriptsize")
        ("linenos" "true")
        ))


(require 'ob-latex)

;; org-trello
;; (require 'org-trello)
;; (add-hook 'org-mode-hook 'org-trello-mode)
;; (add-hook 'org-trello-mode-hook (lambda () (org-trello/install-local-prefix-mode-keybinding! "C-c t")))

(provide 'init-org)
