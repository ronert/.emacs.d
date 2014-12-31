(use-package diminish
  :ensure t
  :pin melpa-stable
  :init
  (diminish 'anzu-mode)
  (diminish 'guide-key-mode)
  (diminish 'google-this-mode)
  (diminish 'projectile-mode)
  (diminish 'eldoc-mode)
  (diminish 'auto-complete-mode)
  (diminish 'whole-line-or-region-mode)
  (diminish 'yas-minor-mode)
  (diminish 'abbrev-mode))

(provide 'init-diminish)
