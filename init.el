(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Set path to dependencies
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path "~/.emacs.d/src/")

;; Is a mac?
(defconst *is-a-mac* (eq system-type 'darwin))

;; Bootstrap config
(setq rr-initialization-errors nil)
(require 'init-bootstrap)


(defvar init-files
  '(
    init-utils ;; Must come before elpa, as it may provide package.el
init-site-lisp
init-elpa      ;; Machinery for installing required packages
init-exec-path ;; Set up $PATH
;; Appearance
init-appearance
;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
    init-smartparens
    init-yasnippet
    init-hippie-expand
    init-bookmarks
    init-helm
    init-eshell
    init-dired
    init-git
    init-ido
    init-ibuffer
    init-search
    init-guidekey
    init-flycheck

    init-packages

    init-defuns
    init-coding

    init-bindings
    init-misc
    init-compile

    ;; specific programming modes modes
    init-csv
    init-ess
    init-haskell
    init-latex
    init-reftex
    init-lisp
    init-markdown
    init-python
    init-stan
    init-org
    init-org-templates
    init-scala
    init-sql
    init-css
    init-javascript
    init-html
    init-hadoop
    init-devops

    ;; company mode
    init-company


    ;; diminish mode line
    init-diminish

    ;; Bindings
    init-bindings
    init-hydra

    ;; Mode mappings
    init-mode-mapping
    ))

(rr-safe-load-init-files)

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
