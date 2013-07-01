;; Make RefTex able to find my local bib files
(setq reftex-bibpath-environment-variables
      '("/Users/ronert/Dropbox/bib"))

(setq reftex-default-bibliography
      (quote
       ("/Users/ronert/Dropbox/bib/library.bib" "/Users/ronert/Dropbox/bib/rpackages.bib")))



(provide 'init-reftex)
