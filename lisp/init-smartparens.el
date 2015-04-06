(use-package smartparens
  :ensure t
  :pin melpa-stable
  ;; highlights matching pairs
  (show-smartparens-global-mode t)
  :config
  (progn
    (use-package smartparens-config)
    (use-package smartparens-latex)
    (smartparens-global-mode t)
    ;; do not autoinsert ' pair if the point is preceeded by word.  This
    ;; will handle the situation when ' is used as a contraction symbol in
    ;; natural language.  Nil for second argument means to keep the
    ;; original definition of closing pair.
    (sp-pair "'" nil :unless '(sp-point-after-word-p))

    ;; emacs is lisp hacking enviroment, so we set up some most common
    ;; lisp modes too
    (sp-with-modes '(
                     emacs-lisp-mode
                     inferior-emacs-lisp-mode
                     lisp-interaction-mode
                     scheme-mode
                     lisp-mode
                     eshell-mode
                     slime-repl-mode
                     clojure-mode
                     common-lisp-mode
                     )
      ;; disable ', it's the quote character!
      (sp-local-pair "'" nil :actions nil)
      ;; also only use the pseudo-quote inside strings where it serve as
      ;; hyperlink.
      (sp-local-pair "`" nil :when '(sp-in-string-p)))

    ;; NOTE: Normally, `sp-local-pair' accepts list of modes (or a single
    ;; mode) as a first argument.  The macro `sp-with-modes' adds this
    ;; automatically.  If you want to call sp-local-pair outside this
    ;; macro, you MUST supply the major mode argument.

    ;; markdown based modes
    (sp-with-modes '(
                     markdown-mode
                     gfm-mode
                     rst-mode
                     )
      ;; overload the `' pair with ``, which is used for inline
      ;; code in markdown
      (sp-local-pair "`" "`"))

    ;; LaTeX modes
    (sp-with-modes '(
                     tex-mode
                     plain-tex-mode
                     latex-mode
                     org-mode
                     )
      ;; math modes, yay.  The :actions are provided automatically if
      ;; these pairs do not have global definition.
      (sp-local-pair "$" "$")
      ;; (sp-local-pair "\\[" "\\]")
      ;; (sp-local-pair "\\{" "\\}")
      (sp-local-pair "„" "“")
      (sp-local-pair "“" "”")
      (sp-local-tag "\\b" "\\begin{_}" "\\end{_}"))

    ;; org-mode
    (sp-with-modes '(
                     org-mode
                     ))

    ;; Haskell
    (sp-with-modes '(
                     haskell-mode
                     inferior-haskell-mode
                     )
      ;; math modes, yay.  The :actions are provided automatically if
      ;; these pairs do not have global definition.
      (sp-local-pair "`" "`"))

    ;; Shells
    (add-hook 'inferior-python-mode-hook (lambda () (smartparens-mode 1)))
    (add-hook 'inferior-ess-mode-hook (lambda () (smartparens-mode 1)))
    (add-hook 'inferior-haskell-mode-hook (lambda () (smartparens-mode 1)))

    ;; html modes
    (sp-local-tag '(sgml-mode html-mode) "<" "<_>" "</_>" :transform 'sp-match-sgml-tags)
    ))


(provide 'init-smartparens)
