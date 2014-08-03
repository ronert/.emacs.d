(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-XeTeX-command "xelatex -synctex=1")
 '(TeX-command-list
   (quote
    (("Latexmk" "latexmk -pdflatex='lualatex --shell-escape' -pdf %t" TeX-run-TeX nil t :help "Run Latexmk on file")
     ("LaTeX" "%`%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("ConTeXt" "texexec --once --texutil %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "texexec %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files"))))
 '(TeX-engine (quote luatex))
 '(TeX-view-program-list (quote (("Preview" "open %o"))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Preview")
     (output-html "xdg-open"))))
 '(ac-auto-start 0)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(auto-indent-disabled-modes-list
   (quote
    (compilation-mode conf-windows-mode diff-mode inferior-ess-mode dired-mode eshell-mode fundamental-mode log-edit-mode makefile-gmake-mode org-mode snippet-mode texinfo-mode text-mode wl-summary-mode yaml-mode nil haskell-mode)))
 '(auto-indent-next-pair-timer-geo-mean (quote ((default 0.0005 0))))
 '(background-color "#fcf4dc")
 '(background-mode light)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(cursor-color "#52676f")
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "284aece21e57abcf7c7d5f273d2d17dc646b24cb1465fd054ad9dca3555aed1c" "b7553781f4a831d5af6545f7a5967eb002c8daeee688c5cbf33bf27936ec18b3" "54d1bcf3fcf758af4812f98eb53b5d767f897442753e1aa468cfeb221f8734f9" "baed08a10ff9393ce578c3ea3e8fd4f8c86e595463a882c55f3bd617df7e5a45" "40eba70ee07212464f2c979fffbaea7d05720aefc4f5795276dc69f83c257469" default)))
 '(electric-pair-skip-self nil)
 '(elpy-rpc-backend "jedi")
 '(elpy-rpc-timeout 1)
 '(ess-roxy-str "##'")
 '(ess-smart-S-assign-key "–")
 '(ess-tracebug-prefix "")
 '(flymake-no-changes-timeout 120)
 '(foreground-color "#52676f")
 '(free-keys-modifiers (quote ("C" "M" "C-M")))
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(hippie-expand-try-functions-list
   (quote
    (try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol yas/hippie-try-expand)))
 '(magit-restore-window-configuration t)
 '(magit-use-overlays nil)
 '(max-specpdl-size 10000)
 '(minimap-update-delay 0.8)
 '(minimap-width-fraction 0.1)
 '(org-agenda-files
   (quote
    ("~/Dropbox/Rasch/Projekt/rasch.org" "~/Dropbox/Statistik/Multivariate Verfahren/multi.org" "~/Dropbox/Statistik/Stochastische Prozesse/stochastische.org" "~/Dropbox/Statistik/Schätzen und Testen II/schaetzen.org" "~/Dropbox/Statistik/Computerintensive Methoden II/computer.org" "/Users/ronert/Dropbox/org/archive.org" "/Users/ronert/Dropbox/org/börse.org" "/Users/ronert/Dropbox/org/diary.org" "/Users/ronert/Dropbox/org/einkaufen.org" "/Users/ronert/Dropbox/org/from-mobile.org" "/Users/ronert/Dropbox/org/index.org" "/Users/ronert/Dropbox/org/it.org" "/Users/ronert/Dropbox/org/personal.org" "/Users/ronert/Dropbox/org/projekte.org" "/Users/ronert/Dropbox/org/refile.org" "/Users/ronert/Dropbox/org/uni.org")))
 '(org-beamer-column-view-format
   "%45ITEM %10BEAMER_env(Env) %10BEAMER_envargs(Env Args) %4BEAMER_col(Col) %8BEAMER_extra(Extra)")
 '(org-beamer-frame-default-options "fragile")
 '(org-beamer-outline-frame-options "")
 '(org-export-run-in-background nil)
 '(org-export-with-todo-keywords nil)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-jsinfo org-irc org-mac-message org-mew org-mhe org-rmail org-vm org-wl org-w3m org-mac-link-grabber)))
 '(preview-TeX-style-dir "/Users/ronert/.emacs.d/elpa/auctex-11.87.2/latex")
 '(projectile-cache-file "/Users/ronert/.projectile.cache")
 '(projectile-known-projects-file "/Users/ronert/projectile-bookmarks.eld")
 '(projectile-remember-window-configs t)
 '(r-autoyas-auto-expand-with-paren t)
 '(reftex-cite-format
   (quote
    ((110 . "\\nocite{%l}")
     (99 . "\\cite[]{%l}")
     (116 . "\\textcite{%l}")
     (97 . "\\autocite[]{%l}")
     (112 . "\\parencite{%l}")
     (102 . "\\footcite[][]{%l}")
     (70 . "\\fullcite[]{%l}")
     (120 . "[]{%l}")
     (88 . "{%l}"))))
 '(reftex-default-bibliography
   (quote
    ("/Users/ronert/Dropbox/bib/library.bib" "/Users/ronert/Dropbox/bib/rpackages.bib" "/Users/ronert/Dropbox/bib/websites.bib")))
 '(reftex-ref-style-alist
   (quote
    (("Default" t
      (("\\ref" 114)
       ("\\pageref" 112)))
     ("Varioref" "varioref"
      (("\\vref" 118)
       ("\\vpageref" 103)
       ("\\Vref" 86)
       ("\\Ref" 82)))
     ("Fancyref" "fancyref"
      (("\\fref" 102)
       ("\\Fref" 70)))
     ("Hyperref" "hyperref"
      (("\\autoref" 97)
       ("\\autopageref" 117)))
     ("Cleveref" "cleveref"
      (("\\cref" 99)
       ("\\Cref" 67))))))
 '(reftex-ref-style-default-list (quote ("Default" "Varioref" "Cleveref")))
 '(reftex-vref-is-default t)
 '(safe-local-variable-values
   (quote
    ((whitespace-style face tabs spaces trailing lines space-before-tab::space newline indentation::space empty space-after-tab::space space-mark tab-mark newline-mark))))
 '(session-use-package t nil (session))
 '(sql-indent-maybe-tab t)
 '(syslog-debug-face
   (quote
    ((t :background unspecified :foreground "#2aa198" :weight bold))))
 '(syslog-error-face
   (quote
    ((t :background unspecified :foreground "#dc322f" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#859900"))))
 '(syslog-info-face
   (quote
    ((t :background unspecified :foreground "#268bd2" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#b58900"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#d33682"))))
 '(syslog-warn-face
   (quote
    ((t :background unspecified :foreground "#cb4b16" :weight bold))))
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(vr/default-regexp-modifiers (quote (:I t :M t :S nil :U nil)))
 '(warning-suppress-types (quote ((undo discard-info))))
 '(wg-morph-on nil)
 '(yas-fallback-behavior (quote return-nil))
 '(yas-snippet-dirs "~/Dropbox/dotfiles/.emacs.d/snippets" nil (yasnippet))
 '(yas-triggers-in-field t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
