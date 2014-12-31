(require 'package)

(setq package-archives
      '("org"         . "http://orgmode.org/elpa/")
      ("gnu"         . "http://elpa.gnu.org/packages/")
      ("elpy" . "http://jorgenschaefer.github.io/packages/")
      ("melpa"   . "http://melpa.org/packages/")
      ("melpa-stable" . "http://stable.melpa.org/packages/")
      )

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '(;; stable packages
          (ace-jump-buffer      . "melpa-stable")
          (ace-jump-mode        . "melpa-stable")
          (ace-link             . "melpa-stable")
          (ag                   . "melpa-stable")
          (anzu                 . "melpa-stable")
          (coffee-mode          . "melpa-stable")
          (company              . "melpa-stable")
          (company-ghc          . "melpa-stable")
          (dash                 . "melpa-stable")
          (diminish             . "melpa-stable")
          (dockerfile-mode      . "melpa-stable")
          (ein                  . "melpa-stable")
          (elisp-slime-nav      . "melpa-stable")
          (ess                  . "melpa-stable")
          (exec-path-from-shell . "melpa-stable")
          (expand-region        . "melpa-stable")
          (flx-ido              . "melpa-stable")
          (flycheck             . "melpa-stable")
          (flycheck-haskell     . "melpa-stable")
          (flycheck-hdevtools   . "melpa-stable")
          (gist                 . "melpa-stable")
          (git-messenger        . "melpa-stable")
          (git-timemachine      . "melpa-stable")
          (google-this          . "melpa-stable")
          (goto-last-change     . "melpa-stable")
          (guide-key            . "melpa-stable")
          (haskell-mode         . "melpa-stable")
          (helm                 . "melpa-stable")
          (helm-ack             . "melpa-stable")
          (helm-descbinds       . "melpa-stable")
          (helm-themes          . "melpa-stable")
          (hi2                  . "melpa-stable")
          (highlight-symbol     . "melpa-stable")
          (hl-sexp              . "melpa-stable")
          (ido-ubiquitous       . "melpa-stable")
          (ido-vertical-mode    . "melpa-stable")
          (js2-mode             . "melpa-stable")
          (json-mode            . "melpa-stable")
          (latex-extra          . "melpa-stable")
          (less-css-mode        . "melpa-stable")
          (magit                . "melpa-stable")
          (markdown-mode        . "melpa-stable")
          (mmm-mode             . "melpa-stable")
          (multiple-cursors     . "melpa-stable")
          (org-trello           . "melpa-stable")
          (paredit              . "melpa-stable")
          (popwin               . "melpa-stable")
          (pretty-mode          . "melpa-stable")
          (project-explorer     . "melpa-stable")
          (projectile           . "melpa-stable")
          (puppet-mode          . "melpa-stable")
          (r-autoyas            . "melpa-stable")
          (rainbow-delimiters   . "melpa-stable")
          (sass-mode            . "melpa-stable")
          (scala-mode2          . "melpa-stable")
          (scss-mode            . "melpa-stable")
          (session              . "melpa-stable")
          (simplezen            . "melpa-stable")
          (skewer-less          . "melpa-stable")
          (skewer-mode          . "melpa-stable")
          (smartparens          . "melpa-stable")
          (smex                 . "melpa-stable")
          (smooth-scrolling     . "melpa-stable")
          (stan-mode            . "melpa-stable")
          (stripe-buffer        . "melpa-stable")
          (tagedit              . "melpa-stable")
          (theme-changer        . "melpa-stable")
          (use-package          . "melpa-stable")
          (vagrant              . "melpa-stable")
          (whole-line-or-region . "melpa-stable")
          (writegood-mode       . "melpa-stable")
          (yasnippet            . "melpa-stable")
          ;; elpy
          (elpa . "elpy")
          ;; org
          )))

;;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


(defvar melpa-exclude-packages
  '(slime)
  "Don't install Melpa versions of these packages.")

;; Don't take Melpa versions of certain packages
(setq package-filter-function
      (lambda (package version archive)
        (and
         (not (memq package '(eieio)))
         (or (not (string-equal archive "melpa"))
             (not (memq package melpa-exclude-packages))))))


;; Load packages
(package-initialize)

;; Load use-package
(require-package 'use-package)

;; Load site-lisp/
(setq elisp-source-dir (concat user-emacs-directory "site-lisp"))
(add-to-list 'load-path elisp-source-dir)
(if (file-exists-p elisp-source-dir)
    (let ((default-directory elisp-source-dir))
      (normal-top-level-add-subdirs-to-load-path)))

(provide 'init-elpa)
