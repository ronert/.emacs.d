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
(require 'init-appearance)


;;----------------------------------------------------------------------------;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require 'init-smartparens)
(require 'init-yasnippet)
(require 'init-auto-complete)
(require 'init-bookmarks)
(require 'init-prog-mode)
(require 'init-csv)
(require 'init-dired)
(require 'init-eshell)
(require 'init-ess)
(require 'init-git)
(require 'init-haskell)
(require 'init-helm)
(require 'init-hippie-expand)
(require 'init-ido)
(require 'init-latex)
(require 'init-lisp)
(require 'init-markdown)
(require 'init-misc)
(require 'init-osx)
(require 'init-packages)
(require 'init-python)
(require 'init-reftex)
(require 'init-search)
(require 'init-sessions)
(require 'init-stan)
(require 'init-org)
(require 'init-org-templates)
(require 'init-scala)
(require 'init-guidekey)
(require 'init-diminish)


;; Map Modes
(require 'init-mode-mapping)

;; Load defuns and bindings
(require 'init-defuns)

(add-to-list 'load-path "~/.emacs.d/init-bindings.el") (require 'init-bindings)


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

;;; init.el ends here
