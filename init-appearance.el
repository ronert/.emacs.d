;; Set default font. First one found is selected.

(defun font-existsp (font)
  "Check to see if the named FONT is available."
  (if (null (x-list-fonts font))
      nil t))

(cond
 ((eq window-system nil) nil)
 ((font-existsp "Menlo")
  (set-face-attribute 'default nil :height 141 :font "PragmataPro"))
 ((font-existsp "Source Code Pro")
  (set-face-attribute 'default nil :height 141 :font "Menlo"))
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
(require 'smooth-scrolling)

;; more smooth efforts.
(setq-default
 scroll-conservatively 0
 scroll-up-aggressively 0.01
 scroll-down-aggressively 0.01)

;; change theme according to time of day
(require 'theme-changer)
(setq calendar-latitude 52.49)
(setq calendar-longitude 13.34)
(require-package 'theme-changer)
(change-theme 'solarized-light 'solarized-dark)

;; Make mouse wheel / trackpad scrolling less jerky
(setq mouse-wheel-scroll-amount '(1
                                  ((shift) . 5)
                                  ((control))))

(provide 'init-appearance)
