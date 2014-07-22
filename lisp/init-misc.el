;; Activate global subword-mode
(global-subword-mode t)

;; Basic preferences
(setq-default
 set-mark-command-repeat-pop t
 compilation-scroll-output t
 grep-highlight-matches t
 grep-scroll-output t)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(delete-selection-mode +1)

;; Copy paste behaviour
(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
                                                                       "Copied line") (list (line-beginning-position) (line-beginning-position
                                                                                                                       2)))))
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))
;; use ssh, not rsh
(setq-default remote-shell-program "ssh")


;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (save-buffer)))

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; ediff - don't start another frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; automatically indenting yanked text if in programming-modes
(defvar yank-indent-modes '(python-mode LaTeX-mode TeX-mode R-mode)
  "Modes in which to indent regions that are yanked (or yank-popped). Only
modes that don't derive from `prog-mode' should be listed here.")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
           (or (derived-mode-p 'prog-mode)
               (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

;; Making occur more useful
(eval-when-compile
  (require 'cl))

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Abbreviations
(setq-default abbrev-mode t)
(setq save-abbrevs t)

(setq windmove-wrap-around t)

;; Base dir
(cd "~/")

(mouse-wheel-mode t)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq visible-bell t
      echo-keystrokes 0.1
      font-lock-maximum-decoration t
      font-lock-verbose nil
      inhibit-startup-message t
      transient-mark-mode t
      ;;  color-theme-is-global t
      delete-by-moving-to-trash t
      shift-select-mode nil
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'forward
      whitespace-style '(trailing lines space-before-tab
                                  indentation space-after-tab)
      whitespace-line-column 100
      ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally
      oddmuse-directory (concat user-emacs-directory "oddmuse")
      xterm-mouse-mode t
      save-place-file (concat user-emacs-directory "places"))

(setq browse-url-browser-function 'browse-url-default-macosx-browser)

(auto-compression-mode t)

(global-font-lock-mode t)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(defalias 'yes-or-no-p 'y-or-n-p)
;; Seed the random-number generator
(random t)

(setq diff-switches "-u")


(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat user-emacs-directory "backups")))))


;; Kill lines
(require 'kill-lines)
(global-set-key (kbd "C-c C-<backspace>") 'kill-lines)

;; Rainbow delimiters
(require-package 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

(require-package 'rainbow-mode)

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; activate pending-delete-mode
(pending-delete-mode t)

;; Keep region when undoing in region
(defadvice undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))

;; Automatically open root files as sudo
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; DEL during isearch should edit the search string, not jump back to the previous result
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(provide 'init-misc)
