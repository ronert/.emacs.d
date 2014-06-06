(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)
(setq dotfiles-dir (file-name-directory (or load-file-name (buffer-file-name))))
(add-to-list 'load-path dotfiles-dir)
(setq package-user-dir (concat dotfiles-dir "elpa"))
(add-to-list 'load-path (expand-file-name
                         "lisp" (expand-file-name
                                 "org" (expand-file-name
                                        "src" dotfiles-dir))))

;; Package Locations
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/")
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Is a mac?
(defconst *is-a-mac* (eq system-type 'darwin))

;; Bootstrap config
(require 'init-utils) ;; Must come before elpa, as it may provide package.el
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH
;; Appearance
(require 'init-appearance)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require 'init-smartparens)
(require 'init-yasnippet)
(require 'init-auto-complete)
(require 'init-hippie-expand)
(require 'init-bookmarks)
(require 'init-helm)
(require 'init-eshell)
(require 'init-dired)
(require 'init-git)
(require 'init-ido)
(require 'init-search)
(require 'init-guidekey)
(require 'init-flycheck)

(require 'init-packages)

(require 'init-defuns)
(require 'init-coding)

(require 'init-bindings)
(require 'init-misc)

;; specific programming modes modes
(require 'init-csv)
(require 'init-ess)
(require 'init-haskell)
(require 'init-latex)
(require 'init-reftex)
(require 'init-lisp)
(require 'init-markdown)
(require 'init-python)
(require 'init-stan)
(require 'init-org)
(require 'init-org-templates)
(require 'init-scala)
(require 'init-sql)
(require 'init-css)
(require 'init-javascript)
(require 'init-html)
(require 'init-hadoop)
(require 'init-devops)

;; diminish mode line
(require 'init-diminish)

;; Bindings
(require 'init-bindings)

;; Mode mappings
(require 'init-mode-mapping)

;; Mac-specific stuff
(when *is-a-mac*
  (require 'init-osx))


;;----------------------------------------------------------------------------;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

;;----------------------------------------------------------------------------;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'init-sessions)
;;; init.el ends here
