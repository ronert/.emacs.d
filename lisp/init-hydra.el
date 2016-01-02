(use-package hydra
  :ensure t
  :config (progn
            ;; hydra zoom
            (defhydra hydra-zoom (global-map "<f2>")
              "zoom"
              ("+" text-scale-increase "in")
              ("-" text-scale-decrease "out"))
            (global-set-key (kbd "<f2>") 'hydra-zoom/body)

            ;; hydra git-gutter
            (defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                                                  :hint nil)
              "
Git gutter:
  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _l_: last hunk        set start _R_evision
"
              ("j" git-gutter:next-hunk)
              ("k" git-gutter:previous-hunk)
              ("h" (progn (goto-char (point-min))
                          (git-gutter:next-hunk 1)))
              ("l" (progn (goto-char (point-min))
                          (git-gutter:previous-hunk 1)))
              ("s" git-gutter:stage-hunk)
              ("r" git-gutter:revert-hunk)
              ("p" git-gutter:popup-hunk)
              ("R" git-gutter:set-start-revision)
              ("q" nil :color blue)
              ("Q" (progn (git-gutter-mode -1)
                          ;; git-gutter-fringe doesn't seem to
                          ;; clear the markup right away
                          (sit-for 0.1)
                          (git-gutter:clear))
               :color blue))
            (global-set-key (kbd "<f10>") 'hydra-git-gutter/body)

            ;; hydra rectangle
            (defun ora-ex-point-mark ()
              (interactive)
              (if rectangle-mark-mode
                  (exchange-point-and-mark)
                (let ((mk (mark)))
                  (rectangle-mark-mode 1)
                  (goto-char mk))))

            (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                                 :color pink
                                                 :post (deactivate-mark))
              "
  ^_k_^     _d_elete    _s_tring
_h_   _l_   _o_k        _y_ank
  ^_j_^     _n_ew-copy  _r_eset
^^^^        _e_xchange  _u_ndo
^^^^        ^ ^         _p_aste
"
              ("h" backward-char nil)
              ("l" forward-char nil)
              ("k" previous-line nil)
              ("j" next-line nil)
              ("e" ora-ex-point-mark nil)
              ("n" copy-rectangle-as-kill nil)
              ("d" delete-rectangle nil)
              ("r" (if (region-active-p)
                       (deactivate-mark)
                     (rectangle-mark-mode 1)) nil)
              ("y" yank-rectangle nil)
              ("u" undo nil)
              ("s" string-rectangle nil)
              ("p" kill-rectangle nil)
              ("o" nil nil))
            (global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)

            ;; latex-math-preview
            (defhydra hydra-latex-math-preview (:color blue :hint nil)
              "
              ^math-preview^
  ^_p_^     _p_review expression
  ^_i_^     _i_nsert symbol
  ^_s_^     _s_save image

"
              ("p" latex-math-preview-expression)
              ("i" latex-math-preview-insert-symbol)
              ("s" latex-math-preview-save-image-file)
              )

            (add-hook 'LaTeX-mode-hook
                      (lambda ()
                        (local-set-key (kbd "C-?") 'hydra-latex-math-preview/body)
                        )
                      )

            ;; YASnippet
            (defhydra hydra-yasnippet (:color blue :hint nil)
              "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
              ("d" yas-load-directory)
              ("e" yas-activate-extra-mode)
              ("i" yas-insert-snippet)
              ("f" yas-visit-snippet-file :color blue)
              ("n" yas-new-snippet)
              ("t" yas-tryout-snippet)
              ("l" yas-describe-tables)
              ("g" yas/global-mode)
              ("m" yas/minor-mode)
              ("a" yas-reload-all))
            (global-set-key (kbd "C-c C-y") 'hydra-yasnippet/body)
            ))

;; pytest
(define-key python-mode-map (kbd "C-c C--")
  (defhydra hydra-pytest (:color blue)
    "pytest"
    ("a" pytest-all "all")
    ("m" pytest-module "module")
    ("o" pytest-one "one")
    ("d" pytest-directory "directory")))


(provide 'init-hydra)
