(require-package 'ess)
(load "ess-site.el")
(add-hook 'ess-mode-hook 'run-starter-kit-coding-hook)
;; TODO put this in own prog mode, use starter kit as templae
(add-hook 'ess-mode-hook 'font-lock-comment-annotations)

;; Make yank and smartparens work propperly on a German keyboard
(defun ess-mode-is-intrusive ()
  ;; Make something work in org-mode:
  ;; (local-unset-key (kbd "something I use"))
  (local-unset-key (kbd "C-y"))
  (local-set-key (kbd "C-z") 'ess-yank)
  (local-unset-key (kbd "\\"))
  (local-set-key (kbd "\\") 'self-insert-command)
  )

(add-hook 'ess-R-post-run-hook 'smartparens-mode)
(add-hook 'ess-R-post-run-hook 'ess-mode-is-intrusive)

;; Make TeX and RefTex aware of Snw and Rnw files
(setq reftex-file-extensions
      '(("Snw" "Rnw" "nw" "tex" ".tex" ".ltx") ("bib" ".bib")))
(setq TeX-file-extensions
      '("Snw" "Rnw" "nw" "tex" "sty" "cls" "ltx" "texi" "texinfo"))

;; Lets you do 'C-c C-c Sweave' from your Rnw file
(add-hook 'Rnw-mode-hook
          (lambda ()
            (add-to-list 'TeX-command-list
                         '("Sweave" "R CMD Sweave %s"
                           TeX-run-command nil (latex-mode) :help "Run Sweave") t)
            (add-to-list 'TeX-command-list
                         '("LatexSweave" "%l %(mode) %s"
                           TeX-run-TeX nil (latex-mode) :help "Run Latex after Sweave") t)
            (setq TeX-command-default "Sweave")))

(setq ess-swv-processor "'knitr")

(setq ess-ask-for-ess-directory nil)
(setq ess-local-process-name "R")
(setq ansi-color-for-comint-mode 'filter)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

(defun my-ess-start-R ()
  (interactive)
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1 nil t))
        (R)
        (set-window-buffer w2 "*R*")
        (set-window-buffer w1 w1name))))
(defun my-ess-eval ()
  (interactive)
  (my-ess-start-R)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
    (call-interactively 'ess-eval-line-and-step)))
(add-hook 'ess-mode-hook
          '(lambda()
             (local-set-key [(shift return)] 'my-ess-eval)))
(add-hook 'inferior-ess-mode-hook
          '(lambda()
             (local-set-key [C-up] 'comint-previous-input)
             (local-set-key [C-down] 'comint-next-input)))
(add-hook 'Rnw-mode-hook
          '(lambda()
             (local-set-key [(shift return)] 'my-ess-eval)))

(require 'ess-tracebug)
(add-hook 'ess-post-run-hook 'ess-tracebug t)

;; turn off fancy comments in ess
(setq ess-fancy-comments nil)

;; shortcuts for setting break points
(add-hook 'R-mode-hook
          (lambda ()
            (local-set-key (kbd "<f5>") 'ess-bp-set)
            (local-set-key (kbd "<f6>") 'ess-bp-kill)
            )
          )

;; set up r-process with correct minor modes
(add-hook 'inferior-ess-mode-hook 'smartparens-mode)
(add-hook 'ess-mode-hook 'ess-mode-is-intrusive)

;; prefer auto-fill to visual line wrap in ESS mode
(add-hook 'ess-mode-hook 'turn-on-auto-fill)
(add-hook 'ess-mode-hook 'highlight-indentation-mode)
(add-hook 'inferior-ess-mode-hook 'turn-on-auto-fill)

;; Dont save .Rhistory eveywhere
(setq ess-history-file nil)

(provide 'init-ess)
