;; Make RefTex able to find my local bib files
(setq reftex-bibpath-environment-variables
      '("~/Dropbox/bib"))

(setq reftex-default-bibliography
      (quote
       ("~/Dropbox/bib/library.bib" "~/Dropbox/bib/rpackages.bib")))



(provide 'init-reftex)
