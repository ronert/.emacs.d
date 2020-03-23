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
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(auto-indent-disabled-modes-list
   (quote
    (compilation-mode conf-windows-mode diff-mode inferior-ess-mode dired-mode eshell-mode fundamental-mode log-edit-mode makefile-gmake-mode org-mode snippet-mode texinfo-mode text-mode wl-summary-mode yaml-mode nil haskell-mode)))
 '(auto-indent-next-pair-timer-geo-mean (quote ((default 0.0005 0))))
 '(background-color "#fcf4dc")
 '(background-mode light)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-color "#52676f")
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "fc6e906a0e6ead5747ab2e7c5838166f7350b958d82e410257aeeb2820e8a07a" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "284aece21e57abcf7c7d5f273d2d17dc646b24cb1465fd054ad9dca3555aed1c" "b7553781f4a831d5af6545f7a5967eb002c8daeee688c5cbf33bf27936ec18b3" "54d1bcf3fcf758af4812f98eb53b5d767f897442753e1aa468cfeb221f8734f9" "baed08a10ff9393ce578c3ea3e8fd4f8c86e595463a882c55f3bd617df7e5a45" "40eba70ee07212464f2c979fffbaea7d05720aefc4f5795276dc69f83c257469" default)))
 '(electric-pair-skip-self nil)
 '(elpy-rpc-backend "jedi")
 '(elpy-rpc-python-command "~/anaconda3/bin/python")
 '(elpy-rpc-timeout 1)
 '(ess-roxy-str "##'")
 '(ess-smart-S-assign-key "–")
 '(ess-tracebug-prefix "")
 '(fci-rule-color "#eee8d5")
 '(flymake-no-changes-timeout 120)
 '(foreground-color "#52676f")
 '(free-keys-modifiers (quote ("C" "M" "C-M")))
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote ghci))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hippie-expand-try-functions-list
   (quote
    (try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol yas/hippie-try-expand)))
 '(magit-diff-use-overlays nil)
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
 '(org-modules (quote (org-bbdb org-bibtex org-docview org-info)))
 '(package-selected-packages
   (quote
    (elpy use-package yaml-mode writegood-mode workgroups2 window-jump whole-line-or-region wgrep-ag visual-regexp-steroids vagrant use-package-chords theme-changer tagedit swiper stripe-buffer stan-snippets ssh sqlup-mode sql-indent solarized-theme smooth-scrolling smex smartparens smart-forward skewer-less session scss-mode scratch scala-mode2 sass-mode rainbow-mode rainbow-delimiters r-autoyas pytest pyimpsort puppet-mode project-explorer pretty-mode pip-requirements pig-snippets pig-mode phi-search paredit paradox multiple-cursors mmm-mode markdown-mode magit lively live-py-mode less-css-mode latex-pretty-symbols latex-math-preview latex-extra json-mode js-comint ido-vertical-mode ido-ubiquitous ido-sort-mtime idle-highlight-mode ibuffer-vc hungry-delete hl-sexp hive highlight-symbol hi2 helm-themes helm-projectile helm-hayoo helm-descbinds helm-c-yasnippet helm-ag helm-ack helm-R hayoo guide-key goto-last-change google-this git-timemachine git-messenger git-gutter gist ghci-completion general-close fullframe free-keys format-sql flycheck-tip flycheck-hdevtools flycheck-haskell fasd exec-path-from-shell ensime elisp-slime-nav eldoc-eval ein dockerfile-mode discover-my-major dired+ diff-hl dash-at-point csv-nav csv-mode css-eldoc crontab-mode company-web company-tern company-statistics company-restclient company-math company-ghc company-edbi company-auctex coffee-mode cdlatex bug-hunter beacon anzu alert ag ace-link)))
 '(paradox-github-token t)
 '(preview-TeX-style-dir "/Users/ronert/.emacs.d/elpa/auctex-11.87.2/latex" t)
 '(projectile-cache-file "/Users/ronert/.projectile.cache")
 '(projectile-known-projects-file "/Users/ronert/projectile-bookmarks.eld")
 '(projectile-remember-window-configs t)
 '(python-indent-guess-indent-offset t)
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
     (88 . "{%l}"))) t)
 '(reftex-default-bibliography
   (quote
    ("/Users/ronert/Dropbox/bib/library.bib" "/Users/ronert/Dropbox/bib/rpackages.bib" "/Users/ronert/Dropbox/bib/websites.bib")) t)
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
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
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
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(text-mode-hook (quote (text-mode-hook-identify)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(vr/default-regexp-modifiers (quote (:I t :M t :S nil :U nil)))
 '(warning-suppress-types (quote ((undo discard-info))))
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(wg-morph-on nil)
 '(wg-switch-on-load t)
 '(yas-fallback-behavior (quote call-other-command))
 '(yas-triggers-in-field t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
