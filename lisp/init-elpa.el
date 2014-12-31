(require 'package)

(setq package-archives
      '(
        ("org"         . "http://orgmode.org/elpa/")
        ("gnu"         . "http://elpa.gnu.org/packages/")
        ("elpy" . "http://jorgenschaefer.github.io/packages/")
        ("melpa"   . "http://melpa.org/packages/")
        "melpa-stable" . "http://stable.melpa.org/packages/"
        ))


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

;; Load site-lisp/
(setq elisp-source-dir (concat user-emacs-directory "site-lisp"))
(add-to-list 'load-path elisp-source-dir)
(if (file-exists-p elisp-source-dir)
    (let ((default-directory elisp-source-dir))
      (normal-top-level-add-subdirs-to-load-path)))

(provide 'init-elpa)
