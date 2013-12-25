;; smart-mode-line
(require-package 'smart-mode-line)
(setq sml/theme 'respectful)
(sml/setup)

;; Theme changer
(require 'theme-changer)
(setq calendar-latitude 52.49)
(setq calendar-longitude 13.34)
(require-package 'theme-changer)
(change-theme 'solarized-light 'solarized-dark)

(provide 'init-mode-line)
