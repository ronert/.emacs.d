;; solarized
(use-package solarized-theme
  :ensure t)

;;Set default font. First one found is selected.
(defun font-existsp (font)
  "Check to see if the named FONT is available."
  (if (null (x-list-fonts font))
      nil t))

(cond
 ((eq window-system nil) nil)
 ((font-existsp "Menlo")
  (set-face-attribute 'default nil :height 141 :font "Menlo"))
 ((font-existsp "Source Code Pro")
  (set-face-attribute 'default nil :height 141 :font "Source Code Pro"))
 ((font-existsp "Consolas")
  (set-face-attribute 'default nil :height 141 :font "Consolas"))
 ((font-existsp "Inconsolata")
  (set-face-attribute 'default nil :height 141 :font "Inconsolata"))
 )

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (blink-cursor-mode -1))

;; Line-spacing
(setq-default line-spacing 1)

;; minimize fringe
(setq-default indicate-empty-lines nil)

;; set theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/elpa")
(setq custom-safe-themes t)
(load-theme 'solarized-dark t)

(global-visual-line-mode t)

;; smooth-scrolling
(use-package smooth-scrolling
  :ensure t
  :pin melpa-stable)

;; more smooth efforts.
(setq-default
 scroll-conservatively 0
 scroll-up-aggressively 0.01
 scroll-down-aggressively 0.01)

;;change theme according to time of day
(use-package theme-changer
  :ensure t
  :pin melpa-stable
  :config (progn
               (setq calendar-latitude 52.49)
               (setq calendar-longitude 13.34)
               (require-package 'theme-changer)
               (change-theme 'solarized-light 'solarized-dark)
               ))


(use-package beacon
  :ensure t
  :config (progn
            (beacon-mode 1)
            (setq beacon-push-mark 35)
            (setq beacon-color "#D3D3De")))



;; Make mouse wheel / trackpad scrolling less jerky
(setq mouse-wheel-scroll-amount '(1
                                  ((shift) . 5)
                                  ((control))))

(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))

;; Disable visual bell
(setq visible-bell nil) ;; The default
(setq ring-bell-function 'ignore)

(provide 'init-appearance)
