;; ess-tracebug.el --- Tracing and debugging facilities for ESS.
;;
;; Filename: ess-tracebug.el
;; Author: Spinu Vitalie
;; Maintainer: Spinu Vitalie
;; Copyright (C) 2010, Spinu Vitalie, all rights reserved.
;; Created: Oct 14 14:15:22 2010
;; Version: 0.2
;; URL:
;; Keywords: debug, watch, traceback, ESS, R
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; Features that might be required by this library:
;;
;;   ESS - required
;;   ido, face-remap, cl -  desirable and are part of default emacs
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;  Ess-tracebug is a package for interactive debugging of R code from
;;ESS and provides such features as:
;;- visual debugging
;;- browser, recover and conditional  breakpoints
;;- watch window and loggers
;;- on the fly  debug/undebug of R functions and methods
;;- highlighting of error source references and easy error navigation
;;- interactive traceback.
;;For a complete description please see the
;;documentation at http://code.google.com/p/ess-tracebug/
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;; Version 0.2
;; New features:
;; 1) Watch window and loggers (http://code.google.com/p/ess-tracebug/#Watch_Window)
;; 2) Conditional Breakpoints (http://code.google.com/p/ess-tracebug/#Conditional_Breakpoints)
;; 3) Debug/undebug  on the fly (http://code.google.com/p/ess-tracebug/#Flag/Unflag_for_Debugging)
;; 4) New recover mode. When user enters the recover command (directly or through recover breakpoint) digit keys
;;are enabled 0-9 for frame navigation; c,q, n also trigger the exit from recover (0) to be compatible with the
;;browser mode.
;;
;; Important changes:
;; 1) ess-traceback and ess-debug are now completely merged into single mode ess-tracebug, but internal code is
;;still divided on conceptual grounds.
;; 2) Input-ring and debug-ring are renamed to forward-ring and backward-ring and merged into joint structure
;;called S-ring (http://code.google.com/p/ess-tracebug/#Work-Flow). The M-c d for backward navigation is replaced
;;by M-c I. M-c d now flags function and methods for debugging.
;; 3) Considerable effort have been made to make the program more stable. Particularly, ess-tracebug now uses
;;internal mechanism to check for availability of R process and do not really on ess commands to communicate to the
;;process. Critical variables, indicating the state of the R process are no longer buffer local but part of process plist.
;;
;;
;; Version 0.1
;; 1) Visual  navigation during the debugging
;; 2) Navigation to errors and debug references in inferior buffer
;; 3) Traceback buffer (M-c `) with links to errors to source code
;; 4) Browser and Recover breakpoints
;; 5) Support for custom breakpoints.
;; 6) On Error action toggling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'face-remap nil t) ;; desirable for scaling of the text in watch buffer
(require 'ido nil t) ;; desirable for debug/undebug at point functionality
(require 'cl) ;; a couple of useful functions
(require 'overlay)

(defvar ess-tracebug-version 0.2)

(defgroup ess-tracebug nil
  "Error navigation and debugging for ESS.
Currently only R is supported."
  :link '(emacs-library-link :tag "Source Lisp File" "ess-tracebug.el")
  :group 'ess
  )

(defvar ess-tracebug-indicator " TB"
  "String to be displayed in mode-line alongside the process
  name. Indicates that ess-traceback-mode is turned on. "
  )

(defvar ess-tracebug-p nil
  "Non nil if ess-tracebug is turned on for current process.
Use `ess-tracebug' function to toggle this variable")
(make-variable-buffer-local 'ess-tracebug-p)
(add-to-list 'minor-mode-alist '(ess-tracebug-p ess-tracebug-indicator))

(defcustom ess-tracebug-command-prefix "\M-c"
  "*Key to be used as prefix in ess-debug command key bindings.

The postfix keys are defined in `ess-tracebug-map'.
The overwritten binding is `capitalize-word' and is bound to 'M-c M-c'.
You can set this to \"M-t\" for example,  which would rebind the
default binding `transpose-words'. In this case make sure to
rebind `M-t` to transpose-words command in the `ess-tracebug-map'."
  :type 'string
  :group 'ess-tracebug)
(defvar ess-tracebug-map
  (let ((map (make-sparse-keymap)))
    (define-prefix-command 'map)
    (define-key map "`" 'ess-show-R-traceback)
    (define-key map "w" 'ess-watch)
    (define-key map "i" 'ess-dbg-goto-input-event-marker)
    (define-key map "I" 'ess-dbg-goto-input-event-marker)
    (define-key map "d" 'ess-dbg-flag-for-debuging)
    (define-key map "u" 'ess-dbg-unflag-for-debugging)
    (define-key map "b" 'ess-bp-set)
    (define-key map "B" 'ess-bp-set-conditional)
    (define-key map "l" 'ess-bp-set-logger)
    (define-key map "t" 'ess-bp-toggle-state)
    (define-key map "k" 'ess-bp-kill)
    (define-key map "K" 'ess-bp-kill-all)
    (define-key map "\C-n" 'ess-bp-next)
    (define-key map "\C-p" 'ess-bp-previous)
    (define-key map "e" 'ess-dbg-toggle-error-action)
    (define-key map "c" 'ess-dbg-easy-command)
    (define-key map "n" 'ess-dbg-easy-command)
    (define-key map "p" 'ess-dbg-easy-command)
    (define-key map "q" 'ess-dbg-easy-command)
    (define-key map "0" 'ess-dbg-easy-command)
    (define-key map "1" 'ess-dbg-easy-command)
    (define-key map "2" 'ess-dbg-easy-command)
    (define-key map "3" 'ess-dbg-easy-command)
    (define-key map "4" 'ess-dbg-easy-command)
    (define-key map "5" 'ess-dbg-easy-command)
    (define-key map "6" 'ess-dbg-easy-command)
    (define-key map "7" 'ess-dbg-easy-command)
    (define-key map "8" 'ess-dbg-easy-command)
    (define-key map "9" 'ess-dbg-easy-command)
    (define-key map "s" 'ess-dbg-source-curent-file)
    (define-key map "\M-c" 'capitalize-word)
    map)
  "Keymap used as a binding for `ess-tracebug-command-prefix' key
 in ESS and iESS mode."
  )


(defun ess-tracebug (&optional arg)
  "Toggle ess-tracebug mode.
With ARG, turn ess-tracebug mode on if and only if ARG is
positive.

This mode adds to ESS the interactive debugging, breakpoint and
error navigation functionality.  Strictly speaking ess-tracebug
is not a minor mode. It integrates globally into ESS and iESS.

The functionality in ess-tracebug is divided on conceptual
grounds in tracing and debugging and could be
activated/deactivate separately with `ess-traceback' and
`ess-debug' respectively.
"
  (interactive "P")
  (ess-force-buffer-current "R process to activate the tracebug mode: ")
  (with-current-buffer (process-buffer (get-process ess-local-process-name))
    (setq arg
          (if arg
              (prefix-numeric-value arg)
            (if ess-tracebug-p -1 1)))
    (if (> arg 0)
        (progn
          (ess-tb-start)
          (ess-dbg-start)
          (message "ess-tracebug mode enabled")
          )
      (ess-tb-stop)
      (ess-dbg-stop)
      (message "ess-tracebug mode disabled")
      )
    )
  )


;;;_* TRACEBACK

(require 'compile)
(defgroup ess-traceback nil
  "Tracing for ESS."
  :link '(emacs-library-link :tag "Source Lisp File" "ess-tracebug.el")
  :group 'ess-tracebug
  :prefix "ess-tb-"
  )


(defface ess-tb-last-input-face
  '((((class grayscale)
      (background light)) (:background "DimGray"))
    (((class grayscale)
      (background dark))  (:background "LightGray"))
    (((class color)
      (background light)) (:overline "medium blue" ))
    (((class color)
      (background dark))  (:overline "deep sky blue" ))
    )
  "Face to highlight currently debugged line."
  :group 'ess-traceback )

(defface ess-tb-last-input-fringe-face
  '((((background light)) (:foreground "medium blue" :overline "medium blue"))
    (((background dark))  (:foreground "deep sky blue" :overline "deep sky blue"))
    )
  "Face for fringe bitmap for last-input position."
  :group 'ess-traceback)

(define-fringe-bitmap 'last-input-arrow
  [#b00011111
   #b00010000
   #b00010000
   #b00010000
   #b00010000
   #b00010000
   #b00010000
   #b00010000
   #b00010000
   #b00010000
   #b11010111
   #b01111100
   #b00111000
   #b00010000] nil nil 'top)

(defvar ess-tb-last-input (make-marker)
  "Marker pointing to the last user input position in iESS buffer.
This is the place where `ess-tb-last-input-overlay' is moved.
Local in iESS buffers with `ess-traceback' mode enabled.")

(defcustom inferior-ess-split-long-prompt t
  "If non-nil, long prompt '> > > > > + + + + > ' is split."
  :group 'ess-traceback)

(defcustom inferior-ess-replace-long+ t
  "If non-nil,  '+ + + + ' containing more than 3 + is replaced by `ess-long+replace'"
  :group 'ess-traceback)

(defcustom ess-long+replace "+ ... + "
  "Replacement used for long + prompt."
  :group 'ess-traceback)

(defmacro ess-copy-key (from-map to-map fun)
  `(define-key ,to-map
     (car (where-is-internal ,fun  ,from-map))
     ,fun
     ))

;;;_ + traceback functions
(defun ess-tb-make-last-input-overlay (beg end)
  "Create an overlay to indicate the last input position."
  (let   ((ove (make-overlay beg end)))
    (overlay-put ove 'before-string
                 (propertize "*last-input-start*"
                             'display (list 'left-fringe 'last-input-arrow 'ess-tb-last-input-fringe-face)))
    (overlay-put ove 'face  'ess-tb-last-input-face)
    (overlay-put ove 'evaporate t)
    ove
    )
  )

(defun ess-tb-start ()
  "Start traceback session "
  (with-current-buffer (process-buffer (get-process ess-current-process-name))
    (if (member ess-dialect '("XLS" "SAS" "STA"))
        (error "Can not activate the debuger for %s dialect" ess-dialect)
      )
    (make-local-variable 'compilation-error-regexp-alist)
    (setq compilation-error-regexp-alist ess-R-tb-regexp-alist)
    (compilation-setup t)
    (setq next-error-function 'ess-tb-next-error-function)
    (make-local-variable 'ess-tb-last-input)
    (make-local-variable 'ess-tb-last-input-overlay)
    (make-local-variable 'compilation-search-path)
    (setq compilation-search-path ess-dbg-search-path)
    (save-excursion
      (goto-char comint-last-input-start)
      (setq ess-tb-last-input (point))
      (setq ess-tb-last-input-overlay (ess-tb-make-last-input-overlay  (point-at-bol) (max (1+ (point)) (point-at-eol))))
      )
    (ad-activate 'ess-eval-region)
    (add-hook 'ess-send-input-hook 'move-last-input-on-send-input t t)
    ;; (ad-activate 'ess-eval-linewise)
    ;; (ad-activate 'inferior-ess-send-input)
    (setq ess-tracebug-p t)
    )
  )

(defun ess-tb-stop ()
  "Stop ess traceback session in the current ess process"
  (with-current-buffer (process-buffer (get-process ess-current-process-name))
    (remove-hook 'ess-send-input-hook 'move-last-input-on-send-input t)
    ;; (ad-deactivate 'inferior-ess-send-input)
    (ad-deactivate 'ess-eval-region)
    ;; (ad-deactivate 'ess-eval-linewise)
    (if (local-variable-p 'ess-tb-last-input-overlay)
        (delete-overlay ess-tb-last-input-overlay))
    (kill-local-variable 'ess-tb-last-input-overlay)
    (kill-local-variable 'ess-tb-last-input)
    (font-lock-remove-keywords nil (compilation-mode-font-lock-keywords))
    (font-lock-fontify-buffer)
    (kill-local-variable 'compilation-error-regexp-alist)
    (kill-local-variable 'compilation-search-path)
    (setq ess-tracebug-p nil)
    ))

(defvar ess-R-tb-regexp-alist '(R R3 R-recover)
  "List of symbols which are looked up in `compilation-error-regexp-alist-alist'.")

(add-to-list 'compilation-error-regexp-alist-alist
             '(R "^.* \\(at \\(.+\\)#\\([0-9]+\\)\\)"  2 3 nil 2 1))
;; (add-to-list 'compilation-error-regexp-alist-alist
;;              '(R2 "\\(?:^ +\\(.*?\\):\\([0-9]+\\):\\([0-9]+\\):\\)"  1 2 nil 2 1))
(add-to-list 'compilation-error-regexp-alist-alist
             '(R3 "\\(?:Error .*: *\n? +\\)\\(.*\\):\\([0-9]+\\):\\([0-9]+\\):"  1 2 3 2 1))
(add-to-list 'compilation-error-regexp-alist-alist
             '(R-recover "^ *[0-9]+: +\\(.+\\)#\\([0-9]+:\\)" 1 2 nil 2 1))

;; (setq ess-R-tb-regexp-alist '(R R2 R3 R-recover))
;; (pop compilation-error-regexp-alist-alist)

(defun ess-show-R-traceback ()
  "Display R traceback and last error message.
Pop up a compilation/grep/occur like buffer. Usual global key
bindings are available \(\\[next-error] and \\[previous-error]\)
for `next-error' and `previous-error' respectively.

You can bound 'no-select' versions of this commands  for convenience:
\(define-key compilation-minor-mode-map [(?n)] 'next-error-no-select\)
\(define-key compilation-minor-mode-map [(?p)] 'previous-error-no-select\)
"
  (interactive)
  (ring-insert ess-dbg-forward-ring (point-marker))
  (ess-force-buffer-current "R process to use: ")
  (let ((trbuf  (get-buffer-create "*ess-traceback*")))
    (setq next-error-last-buffer trbuf)
    (with-current-buffer trbuf
      (setq buffer-read-only nil)
      )
    (ess-command2  "try(traceback(), silent=TRUE);cat(\n\"---------------------------------- \n\", geterrmessage(), fill=TRUE)\n" trbuf)
    (set-buffer trbuf) ;; don't move this before ess-command2! mess up multiple processes'
    (if (string= "No traceback available" (buffer-substring 1 23))
        (message "No traceback available")
      (ess-dirs)
      (message nil)
      (goto-char (point-min))
                                        ;(setq font-lock-defaults '(ess-R-mode-font-lock-keywords)) :todo: solve font-lock
      (make-local-variable 'compilation-error-regexp-alist)
      (setq compilation-error-regexp-alist ess-R-tb-regexp-alist)
      (make-local-variable 'compilation-search-path)
      (setq compilation-search-path ess-dbg-search-path)
      (compilation-minor-mode 1)
                                        ;(use-local-map ess-traceback-minor-mode-map)
      (pop-to-buffer trbuf)
      ;; tracebug keys
      (local-set-key ess-tracebug-command-prefix ess-tracebug-map)
      ;; ess keys
      (local-set-key "\C-c\C-s" 'ess-watch-switch-process)
      (local-set-key "\C-c\C-y" 'ess-switch-to-ESS)
      (local-set-key "\C-c\C-z" 'ess-switch-to-end-of-ESS)
      (setq buffer-read-only nil)
      )
    )
  )

(defun ess-tb-next-error-goto-process-marker ()
  ;; assumes current buffer is the process buffer with compilation enabled
  ;; used in ess-tb-next-error-function
                                        ;  (with-current-buffer (process-buffer (get-process ess-local-process-name)) ; already in comint buffer .. no need
  (comint-goto-process-mark)
  (set-window-point (get-buffer-window) (point))  ;moves the cursor
  ;; FIXME: Should jump to current-debug-position,  but messes the things if in recover
  ;; (when (ess-dbg-is-active)
  ;;   (ess-dbg-goto-current-debug-position)
  ;;   )
  )

(defun ess-tb-next-error-function (n &optional reset)
  "Advance to the next error message and visits the file.
This is the value of `next-error-function' in iESS buffers."
  ;; Modified version of `compilation-next-error-function'.
  (interactive "p")
  (if reset  (goto-char (point-max)))
  (let* ((columns compilation-error-screen-columns) ; buffer's local value
         ;; (proc (or (get-buffer-process (current-buffer))
         ;;                         (error "Current buffer has no process")))
         (last 1) timestamp
         (n (or n 1))
         (beg-pos  ; from where the search for next error starts
          (if (and (>= n 0)
                   (comint-after-pmark-p))
              ess-tb-last-input
            (point)
            )
          )
         (loc (condition-case err
                  (compilation-next-error n  nil beg-pos)
                (error
                 (ess-tb-next-error-goto-process-marker)
                 (error "Passed beyond last reference");(error-message-string err))
                 )))
         (loc (if (or (eq n 0)
                      (> (point) ess-tb-last-input))
                  loc
                (ess-tb-next-error-goto-process-marker)
                (error "Passed beyond last-input marker")))
         (end-loc (nth 2 loc))
         (marker (point-marker))
         )
    (setq compilation-current-error (point-marker)
          overlay-arrow-position (if (bolp)
                                     compilation-current-error
                                   (copy-marker (line-beginning-position)))
          loc (car loc))
    ;; If loc contains no marker, no error in that file has been visited.
    ;; If the marker is invalid the buffer has been killed.
    ;; If the file is newer than the timestamp, it has been modified
    ;; (`omake -P' polls filesystem for changes and recompiles when needed
    ;;  in the same process and buffer).
    ;; So, recalculate all markers for that file.
    (unless (and (nth 3 loc) (marker-buffer (nth 3 loc))
                 ;; There may be no timestamp info if the loc is a `fake-loc'.
                 ;; So we skip the time-check here, although we should maybe
                 ;; change `compilation-fake-loc' to add timestamp info.
                 (nth 4 loc)          ;++
                                        ; VS: here  4th loc is always nil,  and changes are not recoreded
                                        ; can't figure out when the markers (3rd loc) and timestamps (4th loc) are set
                                        ; so recalculate if 4th loc is nil.
                 (or (null (nth 4 loc))
                     (equal (nth 4 loc)
                            (setq timestamp
                                  (with-current-buffer
                                      (marker-buffer (nth 3 loc))
                                    (visited-file-modtime))))))
      (with-current-buffer
          (compilation-find-file marker (caar (nth 2 loc))
                                 (cadr (car (nth 2 loc))))
        (save-restriction
          (widen)
          (goto-char (point-min))
          ;; Treat file's found lines in forward order, 1 by 1.
          (dolist (line (reverse (cddr (nth 2 loc))))
            (when (car line)            ; else this is a filename w/o a line#
              (beginning-of-line (- (car line) last -1))
              (setq last (car line)))
            ;; Treat line's found columns and store/update a marker for each.
            (dolist (col (cdr line))
              (if (car col)
                  (if (eq (car col) -1) ; special case for range end
                      (end-of-line)
                    (compilation-move-to-column (car col) columns))
                (beginning-of-line)
                (skip-chars-forward " \t"))
              (if (nth 3 col)
                  (set-marker (nth 3 col) (point))
                (setcdr (nthcdr 2 col) `(,(point-marker)))))))
        ))
    (compilation-goto-locus marker (nth 3 loc) (nth 3 end-loc))
    (setcdr (nthcdr 3 loc) (list timestamp))
    (setcdr (nthcdr 4 loc) t))
  )

(ess-if-verbose-write "\n<- traceback done")

;;;_ + ADVICING
;;; (needed to implement the last user input functionality)
;;; Complete redefining of  eval-region is needed to avoid messing the debugger.
;;; New eval-region  flushes all blank lines and trailing \n's.
(defun ess-eval-region (start end toggle &optional message)
  "Send the current region to the inferior ESS process.
With prefix argument toggle the meaning of `ess-eval-visibly-p';
this does not apply when using the S-plus GUI, see `ess-eval-region-ddeclient'."
  (interactive "r\nP")
  ;;(untabify (point-min) (point-max))
  ;;(untabify start end); do we really need to save-excursion?
  (ess-force-buffer-current "Process to load into: ")
  (message "Starting evaluation...")

  (if (ess-ddeclient-p)
      (ess-eval-region-ddeclient start end 'even-empty)
    ;; else: "normal", non-DDE behavior:
    (let ((visibly (if toggle (not ess-eval-visibly-p) ess-eval-visibly-p))
          (string (buffer-substring-no-properties start end)))
      (setq string (replace-regexp-in-string
                    "\\(\n+\\s *\\'\\)\\|\\(^\\s *\n\\)" "" string))  ;; delete empty lines and trailing \ns
      (if visibly
          (ess-eval-linewise string)
        (if ess-synchronize-evals
            (ess-eval-linewise string
                               (or message "Eval region"))
          ;; else [almost always!]
          (let ((sprocess (get-ess-process ess-current-process-name)))
            (process-send-string sprocess (concat string "\n")))))))

  (message "Finished evaluation")
  (if (and (fboundp 'deactivate-mark) ess-eval-deactivate-mark)
      (deactivate-mark))
  ;; return value
  (list start end)
  )

;; this advice is probably not very useful. :TOTHINK:
;; (defadvice ess-eval-linewise (around move-last-input)
;;   "Move the `ess-tb-last-input' marker and
;;   `ess-tb-last-input-overlay' to apropriate positions.'"
;;   (ess-force-buffer-current "Process to load into: ")
;;   (let* ((last-input-process (get-process ess-local-process-name))
;;          (last-input-mark (copy-marker (process-mark last-input-process))))
;;     ad-do-it
;;     (with-current-buffer (process-buffer last-input-process)
;;       (when (local-variable-p 'ess-tb-last-input)
;;           (setq ess-tb-last-input last-input-mark)
;;           (goto-char last-input-mark)
;;           (move-overlay ess-tb-last-input-overlay (point-at-bol) (point))
;;           )
;;       )
;;     )
;;   )

                                        ;(ad-activate 'ess-eval-linewise)
                                        ;(ad-unadvise 'ess-eval-linewise)

(defun inferior-ess-move-last-input-overlay ()
  (let ((pbol (point-at-bol))
        (pt (point)) )
    (move-overlay ess-tb-last-input-overlay pbol (max (- pt 2) (+ pbol 2)))
    )
  )

(defadvice  ess-eval-region (around move-last-input)
  "Move the `ess-tb-last-input' marker and
  `ess-tb-last-input-overlay' to apropriate positions.'"
  (ess-force-buffer-current "Process to load into: ")
  (let* ((last-input-process (get-process ess-local-process-name))
         (last-input-mark (copy-marker (process-mark last-input-process))))
    ad-do-it
    (with-current-buffer (process-buffer last-input-process)
      (when (local-variable-p 'ess-tb-last-input) ;; TB might not be active in all processes
        (setq ess-tb-last-input last-input-mark)
        (goto-char last-input-mark)
        (inferior-ess-move-last-input-overlay)
        (comint-goto-process-mark)
        )
      (when (and inferior-ess-split-long-prompt
                 (> (current-column) 2)
                 (looking-back "> "))
        (backward-char 2)
        (insert " \n")
        )
      )
    )
  )

;;(ad-activate 'ess-eval-region)
                                        ; (ad-unadvise 'ess-eval-region)

(defun move-last-input-on-send-input ()
  (setq ess-tb-last-input (point))
  (inferior-ess-move-last-input-overlay)
  )


;; (defadvice inferior-ess-send-input (after move-last-input)
;;   "Move the `ess-tb-last-input' marker and
;;   `ess-tb-last-input-overlay' to apropriate positions.'"
;;   (move-last-input-on-send-input)
;;   )

;; ;;ad-activate 'inferior-ess-send-input)
;; (ad-deactivate 'inferior-ess-send-input)


(ess-if-verbose-write "\n<- advising done")


;;;_* DEBUGER

(defgroup ess-debug nil
  "Debugging for ESS"
  :link '(emacs-library-link :tag "Source Lisp File" "ess-tracebug.el")
  :group 'ess-tracebug
  :prefix "ess-dbg-"
  )


(defvar ess-dbg-error-action "-"
  "Mode line indicator of the current \"on error\" action.
Customize this variable to change the default behavior.
See `ess-dbg-error-action-alist' for more."
  ;; :type 'string
  ;; :group 'ess-debug)
  )

(defcustom  ess-dbg-error-action-alist
  '(( "-" "NONE"       "NULL" )
    ( "r" "RECOVER"    "utils::recover")
    ( "t" "TRACEBACK"  "base::traceback"))
  "Alist of 'on-error' actions.
  Each element must have the form (SYMB DISP ACTION) where DISP
  is the string to be displayed in the mode line when the action
  is in place. SYMB is the symbolic name of an action. ACTION
  is the string giving the actual expression to be assigned to
  'error' user option. See R's help ?options for more details."
  :type '(alist :key-type string
                :value-type (string string))
  :group 'ess-debug)

(defvar ess-dbg-output-buf-prefix " *ess.dbg"
  "The prefix of the buffer name the R debug output is directed to."  )

(defvar ess-dbg-current-ref (make-marker)
  "Current debug reference in *ess.dbg* buffers (a marker).")
(make-variable-buffer-local 'ess-dbg-current-ref)

(defvar ess-dbg-last-ref-marker (make-marker)
  "Last debug reference in *ess.dbg* buffers (a marker).")

;; (defvar ess-dbg-is-active nil
;;   "Indicates whether the debuger is active, i.e. is in the
;;   browser, debuger or alike R session. This variable could be
;;   toggled by [\M-c t]. Might be useful if you want to iterate
;;   manually at R's prompt and debugger's active line not to jump
;;   around.")

(defcustom ess-dbg-search-path '(nil)
  "List of directories to search for source files.
Elements should be directory names, not file names of directories.
"
  :type '(repeat (choice (const :tag "Default" nil)
                         (string :tag "Directory")))
  :group 'ess-debug)


(defvar ess-dbg-buf-p nil
  "This is t in ess.dbg buffers.")
(make-variable-buffer-local 'ess-dbg-buf-p)

(defvar ess-dbg-current-debug-position (make-marker)
  "Marker to the current debugged line.
 It always point to the beginning of the currently debugged line
and is used by overlay-arrow.")

(defface ess-dbg-current-debug-line-face
  '((((class grayscale)
      (background light)) (:background "DimGray"))
    (((class grayscale)
      (background dark))  (:background "LightGray"))
    (((class color)
      (background light)) (:background "tan"))
    (((class color)
      (background dark))  (:background "gray20"))
    )
  "Face used to highlight currently debugged line."
  :group 'ess-debug
  )


(defvar  ess-dbg-current-debug-overlay
  (let ((overlay (make-overlay (point) (point))))
    (overlay-put overlay 'face  'ess-dbg-current-debug-line-face)
    (overlay-put overlay 'evaporate t)
    overlay
    )
  "The overlay for currently debugged line.")


(defcustom ess-dbg-blink-interval .2
  "Time in seconds to blink the background
 of the current debug line on exceptional events.
 Currently two exceptional events are defined 'ref-not-found'
 and 'same-ref'. Blinking colors for these events can be
 customized by corresponding faces."
  :group 'ess-debug
  :type 'float)

(defface ess-dbg-blink-ref-not-found-face
  '((((class grayscale)
      (background light)) (:background "DimGray"))
    (((class grayscale)
      (background dark))  (:background "LightGray"))
    (((class color)
      (background light)) (:background "IndianRed4"))
    (((class color)
      (background dark))  (:background "dark red"))
    )
  "Face used to blink currently debugged line's background
 when the reference file is not found. See also `ess-dbg-ask-for-file'"
  :group 'ess-debug )

(defface ess-dbg-blink-same-ref-face
  '((((class grayscale)
      (background light)) (:background "DimGray"))
    (((class grayscale)
      (background dark))  (:background "LightGray"))
    (((class color)
      (background light)) (:background "steel blue"))
    (((class color)
      (background dark))  (:background "midnight blue"))
    )
  "Face used to highlight currently debugged line when new debug
reference is the same as the preceding one. It is highlighted for
`ess-dbg-blink-interval' seconds."
  :group 'ess-debug )



(defcustom ess-dbg-indicator "db"
  "String to be displayed in mode-line alongside the process
  name. Indicates that ess-debug-mode is turned on. When the
  debuger is in active state this string is showed in upper case
  and highlighted."
  :group 'ess-debug
  :type 'string)


(defcustom ess-dbg-ask-for-file nil
  "If non nil, ask for file if the current debug reference is not found.

If nil, the currently debugged line is highlighted for
`ess-dbg-blink-interval' seconds."
  :group 'ess-debug
  :type 'boolean)

(defvar ess-debug-easy-map
  (let ((map (make-sparse-keymap)))
    (define-prefix-command 'map)
    (define-key map "c" 'ess-dbg-command-c)
    (define-key map "n" 'ess-dbg-command-n)
    (define-key map "p" 'previous-error)
    (define-key map "q" 'ess-dbg-command-Q)
    (define-key map "u" 'ess-dbg-command-u)
    (define-key map "0" 'ess-dbg-command-digit)
    (define-key map "1" 'ess-dbg-command-digit)
    (define-key map "2" 'ess-dbg-command-digit)
    (define-key map "3" 'ess-dbg-command-digit)
    (define-key map "4" 'ess-dbg-command-digit)
    (define-key map "5" 'ess-dbg-command-digit)
    (define-key map "6" 'ess-dbg-command-digit)
    (define-key map "7" 'ess-dbg-command-digit)
    (define-key map "8" 'ess-dbg-command-digit)
    (define-key map "9" 'ess-dbg-command-digit)
    map)
  "Keymap used to define commands for easy input mode.
This commands are triggered by `ess-dbg-easy-command' ."
  )


(defvar ess-dbg-forward-ring (make-ring 10)
  "Ring of markers to the positions of the user inputs
 when the  debugger or traceback events are initiated.  It is used in
 `ess-dbg-goto-input-point'.")

(defvar ess-dbg-backward-ring (make-ring 10)
  "Ring of markers to the positions from which `ess-dbg-goto-input-point' is called.
 See the also `ess-dbg-goto-debug-point'")

(ess-if-verbose-write "\n<- debug-vars done")

;;;_ + debug functions
(defun ess-dbg-set-error-action (spec)
  "Set the on-error action. The ACTION should be  one
of components of `ess-dbg-error-action-alist' (a cons!)."
  (let ((proc (get-process ess-current-process-name)))
    (if spec
        (progn
          (setq ess-dbg-error-action (car spec))
          (ess-command2 (format "options(error= %s )\n" (nth 2 spec) ))
          )
      (error "Unknown action.")
      )
    )
  )



(defun ess-dbg-toggle-error-action ()
  "Toggle the 'on-error' action.
The list of actions are specified in `ess-dbg-error-action-alist'. "
  (interactive)
  (let* ( (alist ess-dbg-error-action-alist)
          (ev last-command-event)
          (com-char  (event-basic-type ev))
          actions act
          )
    (setq actions (cdr (member (assoc ess-dbg-error-action ess-dbg-error-action-alist)
                               ess-dbg-error-action-alist)))
    (unless actions
      (setq actions ess-dbg-error-action-alist))
    (setq act (pop actions))
    (ess-dbg-set-error-action act)
    (message "On-error action set to : %s" (cadr act))
    (while  (eq (setq ev (read-event)) com-char)
      (unless actions
        (setq actions ess-dbg-error-action-alist))
      (setq act (pop actions))
      (ess-dbg-set-error-action act)
      (message "On-error action set to : %s" (cadr act))
      )
    (push ev unread-command-events)
    )
  )

(defun ess-dbg-activate-overlays ()
  "Initialize active debug line."
  (move-overlay ess-dbg-current-debug-overlay (point-at-bol) (1+ (point-at-eol)) (current-buffer))
  (move-marker ess-dbg-current-debug-position (point-at-bol)) ;; used by overlay-arrow,  should be bol
  )

(defun ess-dbg-deactivate-overlays ()
  "Deletes markers and overlays. Overlay arrow stays, indicating the last debug position."
  (delete-overlay ess-dbg-current-debug-overlay)
  ;; overlay-arrow stays, to indicate the last debugged position!!
  )

;;;_ + Work Flow
(defun ess-dbg-goto-input-event-marker ()
  "Jump to the point where the last debugger/traceback etc event occurred.

   Mainly useful during/after debugging, to jump to the place
from where the code was initialy executed.  This is an
easy-command, which means that after the command is triggered a
single key event is enough to navigate through the input-event-S-ring.
If the key-event which triggered the command is Shift modified
the input-event-S-ring is traversed backwards.

The input-event-S-ring is a virtual object which consists of two
rings `ess-dbg-forward-ring' and `ess-dbg-backward-ring' which
are joint at their tops.

See the more info at http://code.google.com/p/ess-tracebug/#Work-Flow
"
  (interactive)
  (let* ((ev last-command-event)
         (com-char  (event-basic-type ev))
         (ring-el 0)
         input-point)
    (if (memq 'shift (event-modifiers ev))
        (setq input-point (ring-ref ess-dbg-backward-ring 0))
      (ring-insert ess-dbg-backward-ring (point-marker)) ;; insert in backward ring ;;todo: check if the marker to this (close by?) position is already in the ring
      (setq input-point (ring-ref ess-dbg-forward-ring 0))
      )
    (when (marker-buffer input-point) ;; todo: give a message here if buff is not found
      (switch-to-buffer (marker-buffer input-point))
      (when (marker-position input-point)
        (goto-char (marker-position input-point))
        ))
    (while  (eq (event-basic-type (setq ev (read-event))) com-char)
      (if (memq 'shift (event-modifiers ev))
          (setq ring-el (1- ring-el))
        (setq ring-el (1+ ring-el))
        )
      (if (< ring-el 0)
          (setq input-point (ring-ref ess-dbg-backward-ring (- ring-el)))  ;; get it from backward-ring
        (setq input-point (ring-ref ess-dbg-forward-ring ring-el)) ;; get it from forward-ring
        )
      (when (marker-buffer input-point)
        (switch-to-buffer (marker-buffer input-point))
        (when (marker-position input-point)
          (goto-char (marker-position input-point))
          )
        )
      )
    (push ev unread-command-events)
    )
  )

(defun ess-dbg-goto-debug-point ()
  "Returns to the debugging position.
Jump to markers stored in `ess-dbg-backward-ring'. If
debug session is active, first jump to current debug line.

This is an easy-command. Shift triggers the opposite traverse
of the ring."
  (interactive)
  (let* ((debug-point (ring-ref ess-dbg-backward-ring 0))
         (ev last-command-event)
         (com-char  (event-basic-type ev))
         (ring-el 0))
    (if (ess-dbg-is-active)
        (progn
          (switch-to-buffer (marker-buffer ess-dbg-current-debug-position))
          (goto-char (marker-position ess-dbg-current-debug-position ))
          (back-to-indentation)
          )
      (switch-to-buffer (marker-buffer debug-point))
      (goto-char (marker-position debug-point))
      )
    (while  (eq (event-basic-type (setq ev (read-event))) com-char)
      (if (memq 'shift (event-modifiers ev))
          (setq ring-el (1- ring-el))
        (setq ring-el (1+ ring-el))
        )
      (setq debug-point (ring-ref ess-dbg-backward-ring ring-el))
      (when (marker-buffer debug-point)
        (switch-to-buffer (marker-buffer debug-point))
        (when (marker-position debug-point)
          (goto-char (marker-position debug-point))
          )
        )
      )
    (push ev unread-command-events)
    )
  )

(defun ess-dbg-insert-in-forward-ring ()
  (interactive)
  "Inserts point-marker into the forward-ring."
  (ring-insert ess-dbg-forward-ring (point-marker))
  (message "Point inserted into the forward-ring")
  )

(defun ess-dbg-start ()
  "Start the debug session.
Add to ESS the interactive debugging functionality, breakpoints,
watch and loggers.  Integrates into ESS and iESS modes by binding
`ess-tracebug-map' to `ess-tracebug-command-prefix' in
`ess-mode-map' and `inferior-ess-mode-map' respectively.
"
  (interactive)
  (let ((dbuff (get-buffer-create (concat ess-dbg-output-buf-prefix "." ess-current-process-name "*"))) ;todo: make dbuff a string!
        (proc (get-ess-process ess-current-process-name)))
    (process-put proc 'dbg-buffer dbuff); buffer were the look up takes place
    (process-put proc 'dbg-active nil)  ; t if the process is in active debug state.
                                        ; Active debug states are usually those, in which prompt start with Browser[d]>
    (process-put proc 'ready t)         ; Indicator if the process is ready for input, set in filters
    (set-process-filter proc 'inferior-ess-dbg-output-filter)
    (with-current-buffer (process-buffer proc)
      (if (member ess-dialect '("XLS" "SAS" "STA"))
          (error "Can not activate the debuger for %s dialect" ess-dialect)
        )
      (setq mode-line-process '(" ["
                                ess-local-process-name
                                " "
                                (:eval (if (process-get (get-buffer-process (current-buffer)) 'dbg-active)
                                           (propertize  (upcase ess-dbg-indicator) 'face '(:foreground "white" :background "red"))
                                         ess-dbg-indicator)
                                       )
                                " "
                                ess-dbg-error-action  ;;tothink: associate with process plist??
                                "]: %s"))
      )
    (with-current-buffer dbuff
      (buffer-disable-undo)
      (setq buffer-read-only nil)
      (make-local-variable 'overlay-arrow-position) ;; indicator for next-error functionality in the *ess.dbg*,  useful??
      (goto-char (point-max))
      (setq ess-dbg-buf-p t  ;; true if in  *ess.dbg* buffer
            ess-dbg-current-ref (point-marker)  ;; used by goto-error functionality
            ess-dbg-last-ref-marker (point-marker)  ;; gives marker to reference of the last debugged line
            )
      ;;      (beginning-of-line)
      (setq buffer-read-only t)
      )
    (define-key ess-mode-map ess-tracebug-command-prefix ess-tracebug-map)
    (define-key inferior-ess-mode-map ess-tracebug-command-prefix ess-tracebug-map)
    ;; Un/Debug at point functionality
    (ess-dbg-inject-un/debug-commands)
    (sleep-for 0.05) ;; not needed  but let it be
    ;; watch functionality
    (ess-watch-inject-commands)
    (sleep-for 0.05)
    )
  )

(defun ess-dbg-stop ()
  "End the debug session.
Kill the *ess.dbg.[R_name]* buffer."
  ;;; process plist is not removed, todo?low priority
  (interactive)
  (let ((proc (get-process ess-current-process-name))) ;;local?
    (with-current-buffer (process-buffer proc)
      (if (member ess-dialect '("XLS" "SAS" "STA"))
          (error "Can not deactivate the debuger for %s dialect" ess-dialect))
      (setq mode-line-process '(" [" ess-local-process-name "] %s"))
      )
    (set-process-filter proc 'inferior-ess-output-filter)
    (kill-buffer (process-get proc 'dbg-buffer))
    (process-put proc 'dbg-buffer nil)
    (process-put proc 'dbg-active nil)
    (process-put proc 'ready t)
    ;; (when (buffer-live-p ess-dbg-buffer)
    ;;   ;; (with-current-buffer ess-dbg-buffer
    ;;   ;;   (set-buffer-modified-p nil)
    ;;   ;;   )
    ;;   (kill-buffer ess-dbg-buffer)
    ;;   )
    )
  ;; unset the map
  (define-key ess-mode-map ess-tracebug-command-prefix nil)
  (define-key inferior-ess-mode-map ess-tracebug-command-prefix nil)
  )


(defun ess-dbg-is-active ()
  "Return t if the current R process is in active debugging state."
  (process-get (get-process ess-current-process-name) 'dbg-active)
  )

(defun ess-dbg-is-recover ()
  "Return t if the current R process is in active debugging state."
  (process-get (get-process ess-current-process-name) 'is-recover)
  )

(setq ess-dbg-regexp-reference "debug at +\\(.+\\)#\\([0-9]+\\):")
(setq ess-dbg-regexp-jump "debug at ")
(setq ess-dbg-regexp-active
      (concat "\\(\\(?:Called from: \\)\\|\\(?:debugging in: \\)\\|\\(?:recover()\\)\\)\\|"
              "\\(\\(?:Browse[][0-9]+\\)\\|\\(?:debug: \\)\\)\\|"
              "\\(^Selection: \\'\\)"))

(defun inferior-ess-dbg-output-filter (proc string)
  "Standard output filter for the inferior ESS process
when `ess-debug' is active. Call `inferior-ess-output-filter'.

Check for activation expressions (defined in
`ess-dbg-regexp-action), when found puts iESS in the debugging state.
If in debugging state, mirrors the output into *ess.dbg* buffer."
  (let* ((is-iess (member major-mode (list 'inferior-ess-mode 'ess-watch-mode)))
         (pbuff (process-buffer proc))
         (dbuff (process-get proc 'dbg-buffer))
         (wbuff (get-buffer ess-watch-buffer))
         (dactive (process-get proc 'dbg-active))
         (input-point (point-marker))
         (match-jump (string-match ess-dbg-regexp-jump string))
         (match-active (string-match ess-dbg-regexp-active string))
         (match-skip (and match-active
                          (match-string 1 string)))
         (match-recover (and match-active
                             (match-string 3 string))) ;; Selection:
         ;;check for main  prompt!! the process splits the output and match-end == nil might indicate this only
         (has-end-prompt (string-match "> +\\'" string))
         ) ; current-buffer is still the user's input buffer here
    (process-put proc 'ready has-end-prompt) ;; in recover also is ready?, no, command2 would not work
    (process-put proc 'is-recover match-recover)
    (if inferior-ess-replace-long+
        (setq string (replace-regexp-in-string "\\(\\+ \\)\\{3\\}\\(\\+ \\)+" ess-long+replace string))
      )
    ;; COMINT
    (comint-output-filter proc string)
    ;; (ordinary-insertion-filter proc string)
    (when (and  has-end-prompt wbuff) ;; refresh only if the process is ready and wbuff exists, (not only in the debugger!!)
      (ess-watch-refresh-buffer-visibly wbuff)
      )
    ;; JUMP to line if debug expression was matched
    (when match-jump
      (with-current-buffer dbuff              ;; insert string in *ess.dbg* buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (concat "|-" string "-|"))
          ))
      (if is-iess
          (save-selected-window  ;; do not pop to the debugging line if in iESS
            (ess-dbg-goto-last-ref-and-mark dbuff t))
        (ess-dbg-goto-last-ref-and-mark dbuff)
        )
      )
    ;; SKIP if needed
    (when (and match-skip) ;; (not dactive))
      (process-send-string proc  "n \n")
      )
    ;; EXIT the debuger
    (when (and dactive
               (not (or match-jump match-active))
               has-end-prompt)
      ;; (with-current-buffer dbuff ;; uncomment to see the value of STRING just before  debugger exists
      ;;   (let ((inhibit-read-only t))
      ;;     (goto-char (point-max))
      ;;     (insert (concat " ---\n " string "\n ---"))
      ;;     ))
      (ess-dbg-deactivate-overlays)
      (process-put proc 'dbg-active nil)
      (message "|<-- exited debug -->|")
      (when wbuff
        (ess-watch-refresh-buffer-visibly wbuff ))
      )
    ;; ACTIVATE the debugger and trigger EASY COMMANDif entered for the first time
    (when (and (not dactive)
               (or match-jump match-active))
      (unless is-iess
        (ring-insert ess-dbg-forward-ring input-point))
      (process-put proc 'dbg-active t)
      (ess-dbg-easy-command t)
      )
    ))


(defun ess-dbg-goto-last-ref-and-mark (dbuff &optional other-window)
  "Open the most recent debug reference, and set all the
necessary marks and overlays.

It's called from `inferior-ess-dbg-output-filter'.  DBUFF must be
the *ess.dbg* buffer associated with the process. If OTHER-WINDOW
is non nil, attempt to open the location in a different window."

  (interactive)
  (let (t-debug-position ref)
    (with-current-buffer  dbuff
      (setq ref  (ess-dbg-get-next-ref -1 (point-max) ess-dbg-last-ref-marker ess-dbg-regexp-reference)) ; sets point at the end of found ref
      (when ref
        (move-marker ess-dbg-last-ref-marker (point-at-eol))
        (move-marker ess-dbg-current-ref ess-dbg-last-ref-marker) ;; each new step repositions the current-ref!
        )
      )
    (when ref
      (if (apply 'ess-dbg-goto-ref other-window ref)
          (progn ;; if referenced  buffer is found
            (setq t-debug-position (copy-marker (point-at-bol)))
            (if (equal t-debug-position ess-dbg-current-debug-position)
                (progn ;; highlights the overlay for ess-dbg-blink-interval seconds
                  (overlay-put ess-dbg-current-debug-overlay 'face 'ess-dbg-blink-same-ref-face)
                  (run-with-timer ess-dbg-blink-interval nil
                                  '(lambda ()
                                     (overlay-put ess-dbg-current-debug-overlay 'face 'ess-dbg-current-debug-line-face)
                                     )
                                  )
                  )
                                        ;else
              (ess-dbg-activate-overlays)
              )
            )
        ;;else, buffer is not found: highlight and give the corresponding message
        (overlay-put ess-dbg-current-debug-overlay 'face 'ess-dbg-blink-ref-not-found-face)
        (run-with-timer ess-dbg-blink-interval nil
                        '(lambda ()
                           (overlay-put ess-dbg-current-debug-overlay 'face 'ess-dbg-current-debug-line-face)
                           )
                        )
        (message "Referenced file %s is not found" (car ref))
        )
      )
    )
  )

(defun ess-dbg-goto-ref (other-window file  &optional line col)
  "Opens the reference given by FILE, LINE and COL,
Try to open in a different window  if OTHER-WINDOW is nil.
Return the buffer if found, or nil otherwise be found.
`ess-dbg-find-file' is used to find the FILE and open the
associated buffer. If FILE is nil returns nil.
"
  (if (stringp line) (setq line (string-to-number line)))
  (if (stringp col) (setq col (string-to-number col)))
  (let ((buffer (ess-dbg-find-file  file))
        (line (or line 1))
        )
    (when buffer
      (if (not other-window)
          (switch-to-buffer buffer t)
        (pop-to-buffer buffer t t))
      (save-restriction  ;; don't want to push-mark so not calling goto-line directly
        (widen)
        (goto-char 1)
        (forward-line (1- line))
        )
      (if col
          (goto-char (+ (point-at-bol) col))
        (back-to-indentation))
      buffer
      )
    )
  )

(defun ess-dbg-find-file (filename &rest directory formats)
  "Find a buffer for file FILENAME.
If FILENAME is not found at all, ask the user where to find it if
`ess-dbg-ask-for-file' is non-nil.  Search the directories in
`ess-dbg-search-path' and in DIRECTORY if supplied.  If DIRECTORY
is relative, it is combined with `default-directory'.FORMATS, if
given, is a list of formats to reformat FILENAME when looking for
it: for each element FMT in FORMATS, this function attempts to
find a file whose name is produced by (format FMT FILENAME)."
  ;; VS: modified version of compilation-find-file
  (or formats (setq formats '("%s")))
  (let ((dirs ess-dbg-search-path)
        (spec-dir (if directory
                      (expand-file-name directory)
                    default-directory)
                  ;; add current dir of ess here :TODO:
                  )
        ;; (ess-get-words-from-vector2 "getwd()\n")))) <- does not work,
        ;; the after-revert-hook calls it recursively! tothink:
        buffer thisdir fmts name)
    (if (file-name-absolute-p filename)
        ;; The file name is absolute.  Use its explicit directory as
        ;; the first in the search path, and strip it from FILENAME.
        (setq filename (abbreviate-file-name (expand-file-name filename))
              dirs (cons (file-name-directory filename) dirs)
              filename (file-name-nondirectory filename)))
    ;; Now search the path.
    (while (and dirs (null buffer))
      (setq thisdir (or (car dirs) spec-dir)
            fmts formats)
      ;; For each directory, try each format string.
      (while (and fmts (null buffer))
        (setq name (expand-file-name (format (car fmts) filename) thisdir)
              buffer (and (file-exists-p name)
                          (find-file-noselect name))
              fmts (cdr fmts)))
      (setq dirs (cdr dirs)))
    (if ess-dbg-ask-for-file
        (save-excursion            ;This save-excursion is probably not right.
          (let* ((pop-up-windows t)
                 (name (read-file-name
                        (format "Find next line in (default %s): "  filename)
                        spec-dir filename t nil
                        ))
                 (origname name))
            (cond
             ((not (file-exists-p name))
              (message "Cannot find file `%s'" name)
              (ding) (sit-for 2))
             ((and (file-directory-p name)
                   (not (file-exists-p
                         (setq name (expand-file-name filename name)))))
              (message "No `%s' in directory %s" filename origname)
              (ding) (sit-for 2))
             (t
              (setq buffer (find-file-noselect name)))))
          )
      )
    buffer);; nil if not found
  )

(defun ess-dbg-get-next-ref (n &optional pt BOUND REG nF nL nC)
  "Move point to the next reference in the *ess.dbg* buffer.

Must be called from *ess.dbg* buffer.
It returns the reference in the form (file line col) /all strings/ ,
or NIL if not found .  Prefix arg N says how many error messages
to move forwards (or backwards, if negative).  Optional arg PT,
if non-nil, specifies the value of point to start looking for the
next message, default to (point).  BOUND is the limiting position
of the search.  REG is the regular expression to search with.  nF
- sub-expression of REG giving the 'file'; defaults to 1.  nL -
giving the 'line'; defaults to 2.  nC - sub-expr giving the
'column'; defaults to 3.
"
  (interactive "p")
  (unless ess-dbg-buf-p
    (error "Not in *ess.dbg* buffer."))
  (setq nF (or nF 1)
        nL (or nL 2)
        nC (or nC 3))
  (or pt (setq pt (point)))
  ;; (message "ess-dbg-last-ref-marker%s vs  pt%s vs point-max%s" ess-dbg-last-ref-marker pt (point-max))
  (goto-char pt)
  (if (search-forward-regexp REG BOUND t n)
      (list (match-string nF) (match-string-no-properties nL) (match-string-no-properties nC))
    nil)
  )

(defun ess-dbg-next-ref-function (n &optional reset)
  "Advance to the next reference and visit the location
given by the reference.  This is the value of
`next-error-function' in *ess.dbg* buffers."
  (interactive "p")
  (if reset
      (set-marker ess-dbg-current-ref ess-dbg-last-ref-marker))
  (let ((loc (ess-dbg-get-next-ref n nil ess-dbg-current-ref))  ;; moves point to next/prev ref if any
                                        ; loc is  (file . line_nr)
        dbuff
        )
    (if loc
        (progn
          (set-marker ess-dbg-current-ref (line-end-position))
          (set-marker overlay-arrow-position (line-beginning-position))
          (setq dbuff (ess-dbg-find-file  (car loc)))
          (switch-to-buffer dbuff)
          (goto-line (cdr loc))
          (move-marker ess-dbg-current-debug-position (line-beginning-position))
          (back-to-indentation)
          )
      (if (>= 0 (or n 1))
          (error "Moved past first debug line")
        (error "Moved past last debug line")
        )
      )
    )
  )

(defun ess-dbg-easy-command (&optional wait)
  "Call commands defined in `ess-debug-easy-map'.
Easy input commands are those, which once executed do not requre
the prefix command for subsequent invocation.

 For example, if the prefix command is 'M-c' and
`ess-dbg-command-n' is bound to 'n' and `ess-dbg-command-c' is
bound to 'c' then 'M-c n n c' executes `ess-dbg-command-n'
twise and `ess-dbg-command-c' once. Any other input not defined
in `ess-debug-easy-map' will cause the exit from easy input mode.
If WAIT is t, wait for next input and ignore the keystroke which
triggered the command."
  (interactive)
  (let* ((ev last-command-event)
         (command (lookup-key ess-debug-easy-map (vector ev))))
    (unless wait
      (call-interactively command))
    (while (setq command
                 (lookup-key ess-debug-easy-map
                             (vector (setq ev (read-event))))
                 )
      (funcall command ev)
      )
    (push ev unread-command-events)
    )
  )



(defun ess-dbg-command-digit (&optional ev)
  "Digit commands in recover mode.
If suplied ev must be a proper key event or a string representing the digit."
  (interactive)
  (unless ev
    (setq ev last-command-event))
  (let* ((ev-char (if (stringp ev)
                      ev
                    (char-to-string (event-basic-type ev))))
         (proc (get-process ess-current-process-name))
         (mark-pos (marker-position (process-mark proc)))
         (comint-prompt-read-only nil)
         (prompt))
    (if (process-get proc 'is-recover)
        (with-current-buffer (process-buffer proc)
          (goto-char mark-pos)
          (setq prompt (delete-and-extract-region  (point-at-bol) mark-pos))
          (insert (concat  prompt ev-char "\n"))
          (process-send-string proc (concat ev-char "\n"))
          (move-marker (process-mark proc) (max-char))
          )
      (message "Recover is not active")
      ))
  )

(defun ess-dbg-command-n (&optional ev)
  "Step next in debug mode.
Equivalent to 'n' at the R prompt."
  (interactive)
  (if (not (ess-dbg-is-active))
      (message "Debugging is not active")
    (if (ess-dbg-is-recover)
        (ess-dbg-command-digit "0") ;; get out of recover mode
      (process-send-string (get-process ess-current-process-name) "\n")
      )
    )
  )

(defun ess-dbg-command-Q (&optional ev)
  "Quits the browser/debug in R process.
 Equivalent to 'Q' at the R prompt."
  (interactive)
  (let ((proc (get-process ess-current-process-name) ))
    (if (not (process-get proc 'dbg-active))
        (message "Debugging is not active")
      (when (ess-dbg-is-recover)
        (ess-dbg-command-digit "0") ; gets out of recover mode
        (ess-wait-for-process proc nil t 1))
      (if (process-get proc 'dbg-active) ; still in debug mode
          (process-send-string proc "Q\n"))
      )
    ))

(defun ess-dbg-command-c (&optional ev)
  "Continue the code execution.
 Equivalent of 'c' at the R prompt."
  (interactive)
  (let ((proc (get-process ess-current-process-name) ))
    (if (not (process-get proc 'dbg-active))
        (message "Debugging is not active")
      (when (ess-dbg-is-recover)
        (ess-dbg-command-digit "0")
        (ess-wait-for-process proc nil t 1)
        ) ;; get out of recover mode
      (if (process-get proc 'dbg-active) ; still in debug mode
          (process-send-string proc "c\n"))
      )
    ))

(defun ess-dbg-set-last-input ()
  "Set the `ess-tb-last-input' to point to the current process-mark"
  (interactive)
  (ess-force-buffer-current "R process to use: ")
  (let* ((last-input-process (get-process ess-local-process-name))
         (last-input-mark (copy-marker (process-mark last-input-process))))
    (with-current-buffer (process-buffer last-input-process)
      (when (local-variable-p 'ess-tb-last-input) ;; TB might not be active in all processes
        (setq ess-tb-last-input last-input-mark)
        (goto-char last-input-mark)
        (inferior-ess-move-last-input-overlay)
        (comint-goto-process-mark)
        )
      )
    )
  )

(defun ess-dbg-source-curent-file ()
  "Save current file and source it in the .R_GlobalEnv environment."
  ;; make it more elaborate :todo:
  (interactive)
  (unless ess-current-process-name
    (ess-force-buffer-current "R process to use: ")
    )
  (when buffer-file-name
    (save-buffer)
    (save-selected-window
      (ess-switch-to-ESS t)
      )
    (ess-dbg-set-last-input)
    (process-send-string (get-process ess-current-process-name)
                         (concat "\ninvisible(eval({source(file=\"" buffer-file-name
                                 "\")\n cat(\"Sourced: " buffer-file-name "\\n\")}, env=globalenv()))\n")
                         )
    )
  )

;;;_ + BREAKPOINTS

(defface ess-bp-fringe-inactive-face
  '((((class color)
      (background light)) (:foreground "DimGray"))
    (((class color)
      (background dark))  (:foreground "LightGray"))
    )
  "Face used to highlight inactive breakpoints."
  :group 'ess-debug)

(defface ess-bp-fringe-logger-face
  '((((class color)
      (background light)) (:foreground "dark red"))
    (((class color)
      (background dark))  (:foreground "tomato1"))
    )
  "Face used to highlight loggers."
  :group 'ess-debug)

(defface ess-bp-fringe-browser-face
  '((((class color)
      (background light)) (:foreground "medium blue"))
    (((class color)
      (background dark))  (:foreground "deep sky blue"))
    )
  "Face used to highlight 'browser' breakpoints."
  :group 'ess-debug)

(defface ess-bp-fringe-recover-face
  '((((class color)
      (background light)) (:foreground "dark magenta"))
    (((class color)
      (background dark))  (:foreground "magenta"))
    )
  "Face used to highlight 'recover' breakpoints fringe."
  :group 'ess-debug)


(defcustom ess-bp-type-spec-alist
  '((browser "browser()" "B>\n"   filled-square  ess-bp-fringe-browser-face)
    (recover "browser();recover()" "R>\n"   filled-square  ess-bp-fringe-recover-face)
    )
  "List of lists of breakpoint types.
Each sublist  has five elements:
1- symbol giving the name of specification
2- R expression to be inserted
3- string to be displayed instead of the expression
4- fringe bitmap to use
5- face for fringe and displayed string."
  :group 'ess-debug
  :type '(alist :key-type symbol
                :value-type (group string string symbol face)
                )
  )

(defcustom ess-bp-inactive-spec
  '(inactive     "##"    filled-square  ess-bp-fringe-inactive-face)
  "List giving the inactive breakpoint specifications."
  ;; List format is identical to that of the elements of
  ;; `ess-bp-type-spec-alist' except that the second element giving
  ;; the R expression is meaningless here." ;;fixme: second element is missing make it nil for consistency with all other specs
  :group 'ess-debug)

(defcustom ess-bp-conditional-spec
  '(conditional     "browser(expr={%s})"  "CB[ %s ]>\n"  question-mark  ess-bp-fringe-browser-face)
  "List giving the conditional breakpoint specifications.
List format is identical to that of the elements of
`ess-bp-type-spec-alist'.  User is asked for the conditional
expression to be replaced instead of %s in the second and third
elements of the specifications."
  :group 'ess-debug)

(defcustom ess-bp-logger-spec
  '(logger     ".ess_log_eval('%s')"  "L[ \"%s\" ]>\n"  hollow-square  ess-bp-fringe-logger-face)
  "List giving the loggers specifications.
List format is identical to that of `ess-bp-type-spec-alist'."
  :group 'ess-debug)


(defun ess-bp-create (type &optional condition)
  "Set breakpoint for the current line.
 Returns the begging position of the hidden text."
  (let* ((spec-alist (cond
                      ((eq type 'conditional)
                       (let ((tl (copy-sequence  ess-bp-conditional-spec)))
                         (when (eq (length condition) 0)
                           (setq condition "TRUE"))
                         (setcar (cdr tl) (format (cadr tl) condition))
                         (setcar (cddr tl) (format (caddr tl) condition))
                         (list tl)))
                      ((eq type 'logger)
                       (let ((tl (copy-sequence ess-bp-logger-spec)))
                         (when (eq (length condition) 0)
                           (setq condition "watchLog"))
                         (setcar (cdr tl) (format (cadr tl) condition))
                         (setcar (cddr tl) (format (caddr tl) condition))
                         (list tl)))
                      (t ess-bp-type-spec-alist)))
         (bp-specs (or (assoc type spec-alist)
                       (error "Undefined breakpoint type %s" type)))
         (init-pos (point-marker))
         (fringe-bitmap (nth 3 bp-specs))
         (fringe-face (nth 4 bp-specs))
         (displ-string (nth 2 bp-specs))
         (bp-command (concat  (nth 1 bp-specs) "##:ess-bp-end:##\n"))
         (bp-length (length bp-command))
         (dummy-string (concat "##:ess-bp-start::" (prin1-to-string (car bp-specs)) ":##\n"))
         (dummy-length (length dummy-string))
         insertion-pos
         )
    (set-marker init-pos (1+ init-pos))
    (setq displ-string (propertize displ-string
                                   'face fringe-face
                                   'font-lock-face fringe-face))
    (setq bp-command (propertize bp-command
                                 'ess-bp t
                                 'intangible 'ess-bp
                                 'rear-nonsticky '(intangible ess-bp bp-type)
                                 'bp-type type
                                 'bp-substring 'command
                                 'display displ-string
                                 ))
    (setq dummy-string (propertize dummy-string
                                   'ess-bp t
                                   'intangible 'ess-bp
                                   'bp-type type
                                   'bp-substring 'dummy
                                   'display (list 'left-fringe fringe-bitmap fringe-face)
                                   ))
    (back-to-indentation)
    (setq insertion-pos (point) )
    (insert (concat   dummy-string bp-command))
    (indent-for-tab-command)
    (goto-char (1- init-pos))  ;; sort of save-excursion
    insertion-pos
    ))

(defun ess-bp-get-bp-position-nearby ()
  "Return the cons (beg . end) of breakpoint limit points
closest to the current position.  Only currently visible region of the
buffer is searched.  This command is intended for use in
interactive commands like `ess-bp-toggle-state' and `ess-bp-kill'.
Use `ess-bp-previous-position' in programs."
  (interactive)
  (let* ( (pos-end (if (get-char-property (1- (point)) 'ess-bp)
                       (point)
                     (previous-single-property-change (point) 'ess-bp nil (window-start))))
          (pos-start (if (get-char-property (point) 'ess-bp)    ;;check for bobp
                         (point)
                       (next-single-property-change (point) 'ess-bp nil (window-end))))
          pos dist-up dist-down)
    (if (not (eq pos-end (window-start)))
        (setq dist-up (- (point) pos-end))
      )
    (if (not (eq pos-start (window-end)))
        (setq dist-down (- pos-start (point)))
      )
    (if (and dist-up dist-down)
        (if (< dist-up dist-down)
            (cons (previous-single-property-change pos-end 'ess-bp nil (window-start)) pos-end)
          (cons pos-start (next-single-property-change pos-start 'ess-bp nil (window-end))))
      (if dist-up
          (cons (previous-single-property-change pos-end 'ess-bp nil (window-start)) pos-end)
        (if dist-down
            (cons pos-start (next-single-property-change pos-start 'ess-bp nil (window-end)))
          )
                                        ;(message "No breakpoints in the visible area"))
        ))))


(defun ess-bp-previous-position ()
  "Returns the cons (beg . end) of breakpoint limit points closest
to the current position, nil if not found. "
  (let* ( (pos-end (if (get-char-property (1- (point)) 'ess-bp)
                       (point)
                     (previous-single-property-change (point) 'ess-bp ))))
    (if pos-end
        (cons (previous-single-property-change pos-end 'ess-bp) pos-end)
      )))

(defun ess-bp-set ()
  (interactive)
  (let* ((pos (ess-bp-get-bp-position-nearby))
         (same-line (if pos
                        (and (<=  (point-at-bol) (cdr pos)) (>= (point-at-eol) (car pos)))))
         (types ess-bp-type-spec-alist)
         (ev last-command-event)
         (com-char  (event-basic-type ev))
         bp-type
         )
    (when same-line
      ;; set bp-type to next type in types
      (setq bp-type (get-text-property (car pos) 'bp-type))
      (setq types (cdr (member (assq bp-type types) types))) ; nil if bp-type is last in the list
      (if (null types) (setq types ess-bp-type-spec-alist))
      (ess-bp-kill)
      (indent-for-tab-command)
      )
    (setq bp-type (pop types))
    (ess-bp-create (car bp-type))
    (while  (eq (setq ev (read-event)) com-char)
      (if (null types) (setq types ess-bp-type-spec-alist))
      (setq bp-type (pop types))
      (ess-bp-kill)
      (ess-bp-create (car bp-type))
      (indent-for-tab-command)
      )
    (push ev unread-command-events)
    ))


(defun ess-bp-set-conditional (condition)
  (interactive "sCondition : ")
  (ess-bp-create 'conditional condition)
  (indent-for-tab-command)
  )

(defun ess-bp-set-logger (name)
  (interactive "sLogger name : ")
  (ess-bp-create 'logger name)
  (indent-for-tab-command)
  )

(defun ess-bp-kill ()
  "Remove the breakpoint nearby"
  (interactive)
  (let ((pos (ess-bp-get-bp-position-nearby))
        (init-pos (make-marker)))
    (if (null pos)
        (if (called-interactively-p) (message "No breakpoint nearby"))
      (set-marker init-pos (1+ (point)))
      (goto-char (car pos))
      (delete-region (car pos) (cdr pos))
      (indent-for-tab-command)
      (goto-char (1- init-pos))
      (if (eq (point) (point-at-eol)) (forward-char))
      ))
  )

(defun ess-bp-kill-all nil
  "Delete all breakpoints in current buffer."
  (interactive)
  (let ((count 0)
        (init-pos (make-marker))
        pos)
    (set-marker init-pos (1+ (point)))
    (save-excursion   ;; needed if error
      (goto-char (point-max))
      (while (setq pos (ess-bp-previous-position))
        (goto-char (car pos))
        (delete-region (car pos) (cdr pos))
        (indent-for-tab-command)
        (setq count (1+ count))
        )
      (message "Killed %d breakpoints" count)
      )
    (goto-char (1- init-pos))
    ))


(defun ess-bp-toggle-state ()
  "Toggle the breakpoint between active and inactive states."
  (interactive)
  (save-excursion
    (let ((pos (ess-bp-get-bp-position-nearby))
          (fringe-face (nth 3 ess-bp-inactive-spec))
          (inhibit-point-motion-hooks t)  ;; deactivates intangible property
          beg-pos-dummy end-pos-comment bp-specs)
      (if (null pos)
          (message "No breakpoints in the visible region")
        (goto-char (car pos))
        (setq beg-pos-command (previous-single-property-change (cdr pos) 'bp-substring nil (car pos)))
        (goto-char beg-pos-command)
        (if (equal (get-char-property (1- beg-pos-command) 'bp-substring) 'comment)
            (progn
              ;; not use beg-pos-command here ## is deleted
              (delete-region (previous-single-property-change beg-pos-command 'bp-substring nil (car pos)) beg-pos-command)
              (setq bp-specs (assoc (get-text-property (point) 'bp-type) ess-bp-type-spec-alist))
              (put-text-property  (car pos) (point)
                                  'display (list 'left-fringe (nth 3 bp-specs) (nth 4 bp-specs)))
              )
          (put-text-property  (car pos) beg-pos-command   ;; dummy display change
                              'display (list 'left-fringe (nth 2 ess-bp-inactive-spec) fringe-face))
          (insert (propertize "##"
                              'ess-bp t
                              'intangible 'ess-bp
                              'display (propertize (nth 1 ess-bp-inactive-spec) 'face fringe-face)
                              'bp-type (get-char-property (point) 'bp-type)
                              'bp-substring 'comment
                              )))))))


(defun ess-bp-make-visible ()
  "Makes bp text visible."
  (interactive)
  (let ((pos (ess-bp-get-bp-position-nearby)))
    (set-text-properties (car pos) (cdr pos) (list 'display nil))
    )
  )



(defun ess-bp-next nil
  "Goto next breakpoint."
  (interactive)
  (let ((cur-pos (point))
        (bp-pos (next-single-property-change (point) 'ess-bp)))
    (if bp-pos
        (goto-char bp-pos)
      ;; else start searching from the beggining of buffer
      (setq bp-pos (next-single-property-change (point-min) 'ess-bp nil (point)))
      (if (equal bp-pos (point))
          (message "No breakpoints found")
        (goto-char bp-pos))
      )
    )
  )


(defun ess-bp-previous nil
  "Goto previous breakpoint."
  (interactive)
  (let ((cur-pos (point))
        (bp-pos (previous-single-property-change (point) 'ess-bp)))
    (if bp-pos
        (goto-char bp-pos)
      ;; else start searching from the end of buffer
      (setq bp-pos (previous-single-property-change (point-max) 'ess-bp nil (point)))
      (if (equal bp-pos (point))
          (message "No breakpoints found")
        (goto-char bp-pos))
      )
    )
  )

;;;_ + WATCH

(defun ess-watch-inject-commands ()
  (ess-command2 "
if(!exists('.ess_watch_expressions')){
   assign('.ess_watch_expressions', list(), envir = .GlobalEnv)
}
assign('.ess_watch_eval', function(){
    if(!exists('.ess_watch_expressions')){
        assign('.ess_watch_expressions', list(), envir = .GlobalEnv)
    }
    if(length(.ess_watch_expressions) == 0L){
        cat('\n# Watch list is empty!\n
# a       append new expression
# i       insert new expression
# k       kill
# e       edit the expression
# r       rename
# n/p     navigate
# u/U     move the expression up/down
# q       kill the buffer
')
    }else{
        .parent_frame <- parent.frame()
        .essWEnames <- allNames(.ess_watch_expressions)
        len0p <- !nzchar(.essWEnames)
        .essWEnames[len0p] <- seq_along(len0p)[len0p]
        for(i in seq_along(.ess_watch_expressions)){
            cat('\n@---- ', .essWEnames[[i]], ' ', rep.int('-', max(0, 35 - nchar(.essWEnames[[i]]))), '-@\n', sep = '')
            cat( paste('@---:', deparse(.ess_watch_expressions[[i]][[1L]])), ' \n', sep = '')
            tryCatch(print(eval(.ess_watch_expressions[[i]], envir = .parent_frame)),
                     error = function(e) cat('Error:', e$message, '\n' ),
                     warning = function(w) cat('warning: ', w$message, '\n' ))
        }}
}, envir = .BaseNamespaceEnv); environment(.ess_watch_eval)<-.GlobalEnv;

assign('.ess_log_eval',  function(log_name){
    if(!exists(log_name, envir = .GlobalEnv, inherits = FALSE))
        assign(log_name, list(), envir = .GlobalEnv)
    log <- get(log_name, envir = .GlobalEnv, inherits = FALSE)
    .essWEnames <- allNames(.ess_watch_expressions)
    cur_log <- list()
    .parent_frame <- parent.frame()
    for(i in seq_along(.ess_watch_expressions)){
        capture.output({
        cur_log[[i]] <-
            tryCatch(eval(.ess_watch_expressions[[i]]), envir = .parent_frame,
                     error = function(e) paste('Error:', e$message, '\n'),
                     warning = function(w) paste('warning: ', w$message, '\n'))
        if(is.null(cur_log[i][[1]]))
            cur_log[i] <- list(NULL)
                   })
    }
    names(cur_log) <- .essWEnames
    assign(log_name, c(log, list(cur_log)), envir = .GlobalEnv)
    invisible(NULL)
}, envir = .BaseNamespaceEnv)
environment(.ess_log_eval) <- .GlobalEnv
"))


(defvar ess-watch-command
  ;; assumes that every expression is a structure of length 1 as returned by parse.
  ".ess_watch_eval()\n")

(define-fringe-bitmap 'current-watch-bar
  [#b00001100] nil nil '(top t))

(defun ess-watch-mode ()
  "Major mode for output from `ess-rdired'.
`ess-rdired' provides a dired-like mode for R objects.  It shows the
list of current objects in the current environment, one-per-line.  You
can then examine these objects, plot them, and so on.
\\{ess-rdired-mode-map}"
  (let ((cur-block (max 1 (ess-watch-block-at-point)))
        (dummy-string
         (propertize "*df*" 'display '(left-fringe current-watch-bar font-lock-keyword-face)))
        )
    (kill-all-local-variables )
    (make-local-variable 'revert-buffer-function)
    (setq revert-buffer-function 'ess-watch-revert-buffer)
    (use-local-map ess-watch-mode-map)
    (setq major-mode 'ess-watch-mode)
    (setq mode-name (concat "watch " ess-current-process-name))
    (setq font-lock-defaults
          '(inferior-ess-font-lock-keywords nil nil ((?' . "."))))
    (turn-on-font-lock)
    (setq ess-watch-current-block-overlay
          (make-overlay (point-min) (point-max)))
    (overlay-put ess-watch-current-block-overlay 'line-prefix dummy-string)
    (overlay-put ess-watch-current-block-overlay 'face 'ess-watch-current-block-face)
    (ess-watch-set-current cur-block) ;;
    (when (featurep 'face-remap)
      ;; scale the font
      (setq text-scale-mode-amount ess-watch-scale-amount)
      (text-scale-mode 1)                                        ;    (text-scale-mode -1) ;;restore to default
      )
    ))

(defun ess-watch ()
  "Run ess-watch mode on R objects.
This is the main function.  See documentation for `ess-watch-mode' though
for more information."
  (interactive)
  (let ((wbuf (get-buffer-create ess-watch-buffer)))
    (set-buffer wbuf)
    (ess-watch-mode)
    (ess-watch-refresh-buffer-visibly wbuf) ;; evals the ess-command and displays the buffer if not visible
    (pop-to-buffer wbuf)
    (set-window-dedicated-p (selected-window) 1) ;; not strongly dedicated
    )
  )

(defvar ess-watch-mode-map nil
  "Keymap for the *R watch* buffer.")

(if ess-watch-mode-map
    ()
  (setq ess-watch-mode-map (make-sparse-keymap))
  (define-key ess-watch-mode-map "?" 'ess-watch-help)
  (define-key ess-watch-mode-map "k" 'ess-watch-kill)
                                        ;  (define-key ess-watch-mode-map "u" 'ess-watch-undelete)
  ;; editing requires a little more work.
  (define-key ess-watch-mode-map "a" 'ess-watch-add)
  (define-key ess-watch-mode-map "i" 'ess-watch-insert)
  (define-key ess-watch-mode-map "e" 'ess-watch-edit-expression)
  (define-key ess-watch-mode-map "r" 'ess-watch-rename)
  (define-key ess-watch-mode-map "q" 'ess-watch-quit)
  (define-key ess-watch-mode-map "u" 'ess-watch-move-up)
  (define-key ess-watch-mode-map "U" 'ess-watch-move-down)
  (define-key ess-watch-mode-map "n" 'ess-watch-next-block)
  (define-key ess-watch-mode-map "p" 'ess-watch-previous-block)
  ;; R mode keybindings.
  (define-key ess-watch-mode-map "\C-c\C-s" 'ess-watch-switch-process)
  (define-key ess-watch-mode-map "\C-c\C-y" 'ess-switch-to-ESS)
  (define-key ess-watch-mode-map "\C-c\C-z" 'ess-switch-to-end-of-ESS)
  (define-key ess-watch-mode-map "g" 'revert-buffer)
  ;; Debug keys:
  (define-key ess-watch-mode-map ess-tracebug-command-prefix ess-tracebug-map)
  )

(defface ess-watch-current-block-face
  '(
    ;; (((class grayscale)
    ;;   (background light)) (:background "DimGray"))
    ;; (((class grayscale)
    ;;   (background dark))  (:background "LightGray"))
    ;; (((class color)
    ;;   (background light)) (:background "tan"))
    (((class color)
      (background dark))  (:background "gray10"))
    )
  "Face used to highlight currently debugged line."
  :group 'ess-debug
  )

(defvar  ess-watch-current-block-overlay nil
  "The overlay for currently selected block in the R watch buffer .")
(make-variable-buffer-local 'ess-watch-current-block-overlay)

(defvar ess-watch-buffer "*R watch*"
  "Name of the watch buffer.")

(defvar  ess-watch-start-block "@----"  ;; fixme: make defcustom and modify the injected command correspondingly
  "String indicating the beginning of a block in watch buffer."
  ;; :group 'ess-debug
  ;; :type 'string
  )

(defvar ess-watch-start-expression "@---:"
  "String indicating the beginning of an R expression in watch buffer."
  ;; :group 'ess-debug
  ;; :type 'string
  )

(defcustom ess-watch-height-threshold 40
  "Minimum height for splitting *R* windwow sensibly to make space for watch window.
Has exactly the same meaning and initial value as `split-height-threshold'."
  :group 'ess-debug
  :type 'integer)

(defcustom ess-watch-width-threshold 70
  "Minimum width for splitting *R* windwow sensibly to make space for watch window.
Has exactly the same meaning and initial value as `split-width-threshold'."
  :group 'ess-debug
  :type 'integer)

(defcustom  ess-watch-scale-amount -1
  "The number of steps to scale the watch font down (up).
Each step scales the height of the default face in the watch
window by the variable `text-scale-mode-step' (a negative number
of steps decreases the height by the same amount)")

(defun ess-watch-block-limits-at-point ()
  "Return start and end positions of the watch block."
  (interactive)
  (save-excursion
    (let ((curr (point))
          start-pos end-pos)
      (end-of-line)
      (setq start-pos
            (if (re-search-backward ess-watch-start-block nil t )
                (point)
              (point-min)))
      (goto-char curr)
      (beginning-of-line)
      (setq end-pos
            (if (re-search-forward ess-watch-start-block nil t)
                (match-beginning 0)
              (point-max)))
      (list start-pos end-pos)
      )))

(defun ess-watch-block-at-point ()
  "return the current block's order count, 0 if no block was found."
  (save-excursion
    (let ((cur-point (point))
          (count 0))
      (goto-char (point-min))
      (while (re-search-forward ess-watch-start-block cur-point t)
        (setq count (1+ count)))
      count
      )))

(defun ess-watch-set-current (nr)
  "Move the overlay over the block with count NR in current watch buffer"
  (goto-char (point-min))
  (re-search-forward ess-watch-start-expression nil t nr)
  (goto-char (match-end 0))
  (apply 'move-overlay ess-watch-current-block-overlay (ess-watch-block-limits-at-point))
  )


(defun ess-watch-make-alist ()
  "Create an association list of expression from current buffer (better be a watch buffer).
Each element of assoc list is of the form (pos name expr) where
pos is an unique integer identifying watch blocks by position,
name is a string giving the name of expression block, expr is a
string giving the actual R expression."
  (interactive)
  (save-excursion
    (let* ((reg-name (concat "^" ess-watch-start-block " *\\(\\S-*\\).*$"))
           (reg-expr (concat "^" ess-watch-start-expression "\\s-*\\(.*\\)$"))
           (reg-all (concat "\\(" reg-name "\\)\n\\(" reg-expr "\\)"))
           (pos 0) wal name expr)
      (goto-char (point-min))
      (while (re-search-forward reg-all nil t)
        (setq pos  (+ 1 pos))
        (setq name (match-string-no-properties 2))
        (setq expr (match-string-no-properties 4))
        (if (not (eq (string-to-number name) 0))  ;;if number of any kind set the name to ""
            (setq name ""))
        (setq wal
              (append wal (list (list pos name expr)))
              )
        )
      wal
      )))

(defun ess-watch-parse-assoc (al)
  "Return a string of the form 'assign(\".ess_watch_expressions\", list(a = parse(expr_a), b= parse(expr_b)), envir = .GlobalEnv)'
ready to be send to R process. AL is an association list as return by `ess-watch-make-alist'"
  (setq tte (concat "assign(\".ess_watch_expressions\", list("
                    (mapconcat '(lambda (el)
                                  (if (> (length  (cadr el) ) 0)
                                      (concat "`" (cadr el) "` = parse(text = '" (caddr el) "')")
                                    (concat "parse(text = '" (caddr el) "')")))
                               al ", ")
                    "), envir = .GlobalEnv)\n"))
  )

(defun ess-watch-install-.ess_watch_expressions ()
  ;; this is used when ever watches are added/deleted/modified in any fashion.
  ;; there is no other way to insert info into R's .ess_watch_expressions object'
  ;; !! assumes R watch being the current buffer, otherwise will most likely install empty list.
  (interactive)
  (process-send-string (get-ess-process ess-current-process-name)
                       (ess-watch-parse-assoc (ess-watch-make-alist)))
  ;;todo: delete the prompt at the end of proc buffer todo: defun ess-process-send-string!!
  (sleep-for 0.05)  ;; need here, if ess-command is used immediately after,  for some weird reason the process buffer will not be changed
  )

(defun ess-watch-revert-buffer (ignore noconfirm)
  "Update the watch buffer
Arguments IGNORE and NOCONFIRM currently not used."
  (ess-watch)
  (message "Watch reverted"))

(defun ess-watch-refresh-buffer-visibly (wbuf &optional sleep no-prompt-check)
  "Eval `ess-watch-command' and direct the output into the WBUF.
Call `ess-watch-buffer-show' to make the buffer visible, without
selecting it.  This function is primarily intended for refreshing
the watch window during the debugging."
  ;; assumes that the ess-watch-mode is on!!
  ;; particularly ess-watch-current-block-overlay is installed
  (interactive)
  (ess-watch-buffer-show wbuf) ;; if visible do nothing
  (with-current-buffer wbuf
    (let ((curr-block (max 1 (ess-watch-block-at-point)))) ;;can be 0 if
      (setq buffer-read-only nil)
      (ess-command2  ess-watch-command wbuf sleep no-prompt-check)
      ;; delete the ++++++> line  ;; not very reliable but works fine so far.
      (goto-char (point-min))
      (delete-region (point-at-bol) (+ 1 (point-at-eol)))
      (ess-watch-set-current curr-block)
      (set-window-point (get-buffer-window wbuf) (point))
      (setq buffer-read-only t)
      )))

(defun ess-watch-buffer-show (buffer-or-name)
  "This is the main function to make watch buffer BUFFER-OR-NAME visible.
If already visible, do nothing.

Currently the only positioning rule implemented si to split the R
process window in half.  The behavior is controlled by
`split-window-sensibly' with parameters `split-height-threshold'
and `split-width-threshold' replaced by
`ess-watch-height-threshold' and `ess-watch-width-threshold'
respectively."
  (interactive)
  (unless (get-buffer-window ess-watch-buffer 'visible)
    (save-selected-window
      (ess-switch-to-ESS t)
      (let* ((split-width-threshold ess-watch-width-threshold)
             (split-height-threshold ess-watch-height-threshold)
             (win (split-window-sensibly (selected-window))))
        (if win
            (set-window-buffer win buffer-or-name)
          (display-buffer buffer-or-name) ;; resort to usual mechanism if could not split
          ))
      )))


(defun ess-watch-quit ()
  "Quit (kill) the watch buffer.
If watch buffer exists, it is displayed during the debug
process. The only way to avoid the display, is to kill the
buffer."
  (interactive)
  (kill-buffer) ;; dedicated, window is deleted unless not the only one
  )

;;;_  + MOTION
(defun ess-watch-next-block (&optional n)
  "Move the overlay over the next block.
Optional N if supplied gives the number of steps forward backward-char."
  (interactive "P")
  (setq n (prefix-numeric-value n))
  (goto-char (overlay-end ess-watch-current-block-overlay))
  (unless (re-search-forward ess-watch-start-expression nil t n)
    (goto-char (point-min)) ;;circular but always moves to start!
    (re-search-forward ess-watch-start-expression nil t 1)
    )
  (apply 'move-overlay ess-watch-current-block-overlay (ess-watch-block-limits-at-point))
  )

(defun ess-watch-previous-block (&optional n)
  "Move the overlay over the previous block.
Optional N if supplied gives the number of backward steps."
  (interactive "P")
  (setq n (prefix-numeric-value n))
  (goto-char (overlay-start ess-watch-current-block-overlay))
  (unless (re-search-backward ess-watch-start-expression nil t n)
    (goto-char (point-max)) ;;circular but always moves to last!
    (re-search-backward ess-watch-start-expression nil t 1)
    )
  (goto-char (match-end 0))
  (apply 'move-overlay ess-watch-current-block-overlay (ess-watch-block-limits-at-point))
  )

;;;_  + BLOCK MANIPULATION and EDITING
(defun ess-watch-rename ()
  "Rename the currently selected watch block. "
  (interactive)
  (end-of-line)
  (unless (re-search-backward ess-watch-start-block nil 1)
    (error "Can not find a watch block"))
  (let ((reg-name (concat ess-watch-start-block " *\\(\\S-*\\).*$"))
        name start end)
    ;; (reg-expr (concat "^" ess-watch-start-expression "\\s-*\\(.*\\)$"))
    ;; (reg-all (concat "\\(" reg-name "\\)\n\\(" reg-expr "\\)"))
    ;; (pos 0) wal name expr)
    (unless (re-search-forward reg-name (point-at-eol) 1)
      (error "Can not find the name substring in the current watch block "))
    (setq name (match-string-no-properties 1))
    (setq start (match-beginning 1))
    (setq end (match-end 1))
    (goto-char start)
    ;; todo: highlight the name in R-watch here
    (setq name (read-string (concat "New name (" name "): ") nil nil name) )
    (setq buffer-read-only nil)
    (delete-region start end)
    (insert name)
    (setq buffer-read-only t)
    (ess-watch-install-.ess_watch_expressions)
    (ess-watch-refresh-buffer-visibly (current-buffer))
    ))

(defun ess-watch-edit-expression ()
  "Edit in the minibuffer the R expression from the current watch block. "
  (interactive)
  (end-of-line)
  (unless (re-search-backward ess-watch-start-block nil 1)
    (error "Can not find a watch block"))
  (let ((reg-expr (concat ess-watch-start-expression " *\\(.*\\)$"))
        expr start end)
    (unless (re-search-forward reg-expr nil 1)
      (error "Can not find an expression string in the watch block"))
    (setq expr (match-string-no-properties 1))
    (setq start (match-beginning 1))
    (setq end (match-end 1))
    (goto-char start)
    ;; todo: highlight the name in R-watch here
    (setq expr (read-string  "New expression: " expr nil expr) )
    (setq buffer-read-only nil)
    (delete-region start end)
    (insert expr)
    (setq buffer-read-only t)
    (ess-watch-install-.ess_watch_expressions)
    (ess-watch-refresh-buffer-visibly (current-buffer))
    ))

(defun ess-watch-add ()
  "Ask for new R expression and name and append it to the end of the list of watch expressions"
  (interactive)
  (let (nr expr name)
    (goto-char (point-max))
    (setq nr (number-to-string (1+ (ess-watch-block-at-point))))
    (setq name nr)
    ;; (setq name (read-string (concat "Name (" nr "):") nil nil nr ))  ;;this one is quite annoying and not really needed than for logging
    (setq expr (read-string "New expression: " nil nil "\"Empty watch!\""))
    (setq buffer-read-only nil)
    (insert (concat "\n" ess-watch-start-block " " name " -@\n" ess-watch-start-expression " " expr "\n"))
    (setq buffer-read-only t)
    (ess-watch-install-.ess_watch_expressions)
    (ess-watch-refresh-buffer-visibly (current-buffer))
    ))

(defun ess-watch-insert ()
  "Ask for new R expression and name and insert it in front of current watch block"
  (interactive)
  (let (nr expr name)
    (setq nr (number-to-string (ess-watch-block-at-point)))
    (setq name nr)
    ;; (setq name (read-string (concat "Name (" nr "):") nil nil nr ))
    (setq expr (read-string "New expression: " nil nil "\"Empty watch!\""))
    (re-search-backward ess-watch-start-block nil 1) ;;point-min if not found
    (setq buffer-read-only nil)
    (insert (concat "\n" ess-watch-start-block " " name " -@\n" ess-watch-start-expression " " expr "\n"))
    (setq buffer-read-only t)
    (ess-watch-install-.ess_watch_expressions)
    (ess-watch-refresh-buffer-visibly (current-buffer))
    ))

(defun ess-watch-move-up ()
  "Move the current block up."
  (interactive)
  (let ((nr (ess-watch-block-at-point))
        wbl)
    (when (> nr 1)
      (setq buffer-read-only nil)
      (setq wbl (apply 'delete-and-extract-region  (ess-watch-block-limits-at-point)))
      (re-search-backward ess-watch-start-block nil t 1) ;; current block was deleted, point is at the end of previous block
      (insert wbl)
      (ess-watch-install-.ess_watch_expressions)
      (ess-watch-refresh-buffer-visibly (current-buffer))
      (setq buffer-read-only t)
      )))


(defun ess-watch-move-down ()
  "Move the current block down."
  (interactive)
  (let ((nr (ess-watch-block-at-point))
        (nr-all (save-excursion (goto-char (point-max))
                                (ess-watch-block-at-point)))
        wbl)
    (when (< nr nr-all)
      (setq buffer-read-only nil)
      (setq wbl (apply 'delete-and-extract-region  (ess-watch-block-limits-at-point)))
      (end-of-line)
      (when (re-search-forward ess-watch-start-block nil 1 1) ;; current block was deleted, point is at the end of previous block or point-max
        (goto-char (match-beginning 0)))
      (insert wbl)
      (ess-watch-install-.ess_watch_expressions)
      (ess-watch-refresh-buffer-visibly (current-buffer))
      (setq buffer-read-only t)
      )))

(defun ess-watch-kill ()
  "Kill the current block"
  (interactive)
  (setq buffer-read-only nil)
  (apply 'delete-region (ess-watch-block-limits-at-point))
  (ess-watch-install-.ess_watch_expressions)
  (ess-watch-refresh-buffer-visibly (current-buffer))
  )

;;;_ + Un/Debug at point

(defun ess-dbg-inject-un/debug-commands () ;; fixme: bug: if no starting new-line,ess-command hangs
  (ess-command2 "
local({
    .ess_dbg_getTracedAndDebugged <-     function(){
    tr_state <- tracingState(FALSE)
    on.exit(tracingState(tr_state))
    generics <- methods::getGenerics()
    all_traced <- c()
    for(i in seq_along(generics)){
        genf <- methods::getGeneric(generics[[i]], package=generics@package[[i]])
        if(!is.null(genf)){ ## might happen !! v.2.13
            menv <- methods::getMethodsForDispatch(genf)
            traced <- unlist(eapply(menv, is, 'traceable', all.names=TRUE))
            if(length(traced) && any(traced))
                all_traced <- c(paste(generics[[i]],':', names(traced)[traced],sep=''), all_traced)
            if(!is.null(tfn<-getFunction(generics[[i]], mustFind=FALSE, where = .GlobalEnv))&&is(tfn,  'traceable')) # if the default is traced,  it does not appear in the menv :()
                all_traced <- c(generics[[i]], all_traced)
        }
    }
    debugged <- apropos('.', mode = 'function')
    ## traced function don't appear here. Not realy needed and would affect performance.
    debugged <- debugged[which(unlist(lapply(debugged, isdebugged) , recursive=FALSE, use.names=FALSE))]
    unique(c(debugged, all_traced))
    }
   .ess_dbg_UntraceOrUndebug <- function(name){
        tr_state <- tracingState(FALSE)
        on.exit(tracingState(tr_state))
        ## name is a name of a function to be undebugged or has a form name:Class1#Class2#Class3 for traced methods
        name <- strsplit(name, ':', fixed = TRUE)[[1]]
        if(length(name)>1){
            ## a method
            fun <- name[[1]]
            sig <- strsplit(paste(name[-1], collapse=''), '#', fixed=TRUE)[[1]]
            untrace(fun, signature = sig)
        }else{
            ## function
            if(is(getFunction(name), 'traceable'))
                untrace(name)
            else
                undebug(name)
        }
    }
    .ess_dbg_UndebugALL <- function(funcs){
        tr_state <- tracingState(FALSE)
        on.exit(tracingState(tr_state))
        invisible(lapply(funcs, .ess_dbg_UntraceOrUndebug))
    }
    inject_env <- .GlobalEnv ##.BaseNamespaceEnv
    environment(.ess_dbg_UndebugALL) <-
        environment(.ess_dbg_UntraceOrUndebug) <-
            environment(.ess_dbg_getTracedAndDebugged) <- .GlobalEnv  ## to see all the funcs
    assign('.ess_dbg_getTracedAndDebugged', .ess_dbg_getTracedAndDebugged, envir= inject_env)
    assign('.ess_dbg_UntraceOrUndebug', .ess_dbg_UntraceOrUndebug, envir= inject_env)
    assign('.ess_dbg_UndebugALL', .ess_dbg_UndebugALL, envir= inject_env)
})
"))



(defun ess-dbg-get-signatures (method)
  "Get signatures for the method METHOD"
  (let ((tbuffer (get-buffer-create " *ess-command-output*")); initial space: disable-undo
        signatures curr-point)
    (save-excursion
      (ess-if-verbose-write (format "ess-get-signatures*(%s).. " method))
      (ess-command2 (concat "showMethods(\"" method "\")\n") tbuffer)
      (message ess-local-process-name)
      (message ess-current-process-name)
      (ess-if-verbose-write " [ok] ..\n")
      (set-buffer tbuffer)
      (goto-char (point-min))
      (if (not (re-search-forward "Function:" nil t))
          (progn (ess-if-verbose-write "not seeing \"Function:\".. \n")
                 (error (buffer-string))
                 ;; (error "Cannot trace  method '%s' (Is it a primitive method which you have already traced?)" method)
                 )
        ;; (setq curr-point (point))
        ;; (while (re-search-forward ", " nil t) ;replace all ", " with  ":" for better redability in completion buffers??
        ;;   (replace-match ":"))
        ;; (goto-char curr-point)
        (while (re-search-forward "^.+$" nil t)
          (setq signatures (cons (match-string-no-properties 0) signatures))))
                                        ;      (kill-buffer tbuffer)
      )
    signatures
    ))

(defvar ess-dbg-use-ido t
  "If non-nil use ido completion for debug/undebug functionality.
`ido-mode' is part of emacs. If you are using different
completions mechanisms such as icicle you should set this to nil.
If you are not using ido `ess-dbg-flag-for-debuging' will
activate the ido mode for the period of completion resulting in a
slight overhead of starting the global mode.")

(defun ess-dbg-flag-for-debuging ()
  "Set the debugging flag on a function.
Ask the user for a function and if it turns to be generic, ask
for signature and trace it with browser tracer."
  (interactive)
  (let ((obj-at-point (word-at-point))
        (tbuffer (get-buffer-create " *ess-command-output*")) ;; output buffer name is hard-coded in ess-inf.el
        (all-functions (ess-get-words-from-vector2 "apropos(\".\", mode = \"function\")\n"))
        (loc-completing-read (if (and ess-dbg-use-ido (featurep 'ido))
                                 (function ido-completing-read)
                               (function completing-read)))
        ufunc signature default-string
        reset-ido out-message
        )
    (when obj-at-point
      (if (member obj-at-point all-functions)
          (setq default-string (concat "(" obj-at-point ")"))
        (setq obj-at-point nil)
        ))
    (when  (and ess-dbg-use-ido
                (featurep 'ido )
                (not ido-mode))
      ;; start an ido mode if needed, completions for methods are difficult without it
      (setq reset-ido t)
      (ido-mode 'buffer))
    (unwind-protect
        (progn
          (setq ufunc
                (funcall loc-completing-read
                         (concat "Debug " default-string ": ")
                         all-functions nil t nil nil obj-at-point ))
          ;; check if is generic
          (if (equal "TRUE"
                     (car (ess-get-words-from-vector2  (concat "as.character(isGeneric(\"" ufunc "\"))\n"))))
              (save-excursion ;; if so, find teh signature
                (setq signature (funcall loc-completing-read
                                         (concat "Method for generic '" ufunc "' : ")
                                         (ess-dbg-get-signatures ufunc) ;;signal error if not found
                                         nil t nil nil "*default*"))
                (if (equal signature "*default*")
                    (progn
                      (ess-command2 (concat "trace(\"" ufunc "\", tracer = browser)\n") tbuffer) ;debug the default ufunc
                      )
                  (ess-command2 (concat "trace(\"" ufunc "\", tracer = browser, signature = c(" signature "))\n") tbuffer)
                  )
                (set-buffer tbuffer)
                (setq out-message (buffer-substring-no-properties (point-min) (point-max))) ;; gives appropriate message or error
                )
            ;; not generic
            (save-excursion
              (ess-command2 (concat "debug(\"" ufunc "\")\n") tbuffer)
              (set-buffer tbuffer)
              (if (= (point-max) 1)
                  (setq out-message (format "Flagged function '%s' for debugging" ufunc))
                (setq out-message (buffer-substring-no-properties (point-min) (point-max))) ;; error occurred
                ))
            ))
      (when reset-ido
        (ido-mode nil))
      )
    (message out-message)
    ))


(defun ess-dbg-unflag-for-debugging ()
  "Prompt for the debugged/traced function or method and undebug/untrace it."
  (interactive)
  (let ((tbuffer (get-buffer-create " *ess-command-output*")); initial space: disable-undo\
        (loc-completing-read (if (and ess-dbg-use-ido (featurep 'ido))
                                 (function ido-completing-read)
                               (function completing-read)))
        reset-ido out-message
        debugged fun def-val)
    (setq debugged (ess-get-words-from-vector2 ".ess_dbg_getTracedAndDebugged()\n"))
    (print debugged)
    (if (eq (length debugged) 0)
        (message "No debugged or traced functions/methods found")
      (when  (and ess-dbg-use-ido
                  (featurep 'ido )
                  (not ido-mode))
        ;; start an ido mode if needed, completions for methods are difficult without it
        (setq reset-ido t)
        (ido-mode 'buffer))
      (unwind-protect
          (progn
            (setq def-val (if (eq (length debugged) 1)
                              (car debugged)
                            "*ALL*"))
            (setq fun (funcall loc-completing-read "Un-debug: " debugged nil t nil nil def-val))
            (if (equal fun "*ALL*" )
                (ess-command2 (concat ".ess_dbg_UndebugALL(c(\"" (mapconcat '(lambda (x) x) debugged "\", \"") "\"))\n") tbuffer)
              (ess-command2 (concat ".ess_dbg_UntraceOrUndebug(\"" fun "\")\n") tbuffer)
              )
            (with-current-buffer  tbuffer
              (if (= (point-max) 1)
                  (setq out-message (format  "Un-debugged '%s' " fun))
                (setq out-message (buffer-substring-no-properties (point-min) (point-max))) ;; untrace info or warning, or error occurred
                )))
        (when reset-ido
          (ido-mode nil)))
      (message out-message)
      )))

;;;_ * ESS inf imperfections

(defun ess-wait-for-process (proc &optional sleep force-redisplay timeout)
  "Wait for TIMEOUT seconds the 'ready property of the process to become non-nil."
  (if sleep (sleep-for sleep)); we sleep here, *and* wait below
  (unless timeout
    (setq timeout 30))
  (accept-process-output proc .5)
  (let ((i .5))
    (while (and (not (process-get proc 'ready))
                (<= i timeout))
      ;; (message (format "wait:%s" (process-get proc 'ready)))
      (accept-process-output proc .5)
      (if force-redisplay (redisplay t))
      (setq i (+ 0.5 i))
      (if (> i timeout)
          (ess-if-verbose-write (format "\nWaited for %s seconds. Process is bussy or waits for user input." timeout)))
      )))


(defun ordinary-insertion-filter2 (proc string)
  "improved version of ess filter"
  ;; (with-current-buffer (process-buffer proc)
  (let (moving)
    (process-put proc 'ready (string-match "> +\\'" string))
    (setq moving (= (point) (process-mark proc)))
    (save-excursion
      ;; Insert the text, moving the process-marker.
      (goto-char (process-mark proc))
      (insert string)
      (set-marker (process-mark proc) (point)))
    (if moving (goto-char (process-mark proc))))
  )

(defun ess-command2 (com &optional buf sleep no-prompt-check)
  "Improved version of `ess-command'. Intended to be used when ess-tracebug is on"
  ;; the ddeclient-p checks needs to use the local-process-name
  (unless buf
    (setq buf (get-buffer-create " *ess-command-output*")))
  (with-current-buffer buf
    (unless ess-local-process-name
      (setq ess-local-process-name ess-current-process-name)))
  ;; local-process-name must be used
  ;; if multiple  processes are used, current-process-name might not be the process of current buffer
  ;; (stumbled on this in mark-for-debug function)
  ;; todo: report to ess-core
  (let* ((sprocess (get-ess-process (or ess-local-process-name
                                        ess-current-process-name)))
         do-sleep end-of-output
         oldpb oldpf oldpm
         )
    (if (ess-ddeclient-p)
        (ess-command-ddeclient com buf sleep)

      ;; else: "normal", non-DDE behavior:
      (save-excursion
        ;; (set-buffer sbuffer)
        (ess-if-verbose-write (format "(ess-command2 %s ..)" com))
        (unless (or (process-get sprocess 'ready) no-prompt-check)
          (ess-error
           "ESS process not ready. Finish your command before trying again."))
        (setq oldpf (process-filter sprocess))
        (setq oldpb (process-buffer sprocess))
        (setq oldpm (marker-position (process-mark sprocess)))
        ;; need the buffer-local values in result buffer "buf":
        (unwind-protect
            (progn
              (set-process-buffer sprocess buf)
              (set-process-filter sprocess 'ordinary-insertion-filter2)
              ;; Output is now going to BUF:
              (save-excursion
                (set-buffer buf)
                (setq do-sleep              ; only now when in sprocess buffer
                      (progn
                        (if sleep (if (numberp sleep) nil (setq sleep 1))) ; t means 1
                        (and ess-cmd-delay sleep)))
                (erase-buffer)
                (set-marker (process-mark sprocess) (point-min))
                (process-send-string sprocess com)
                (if no-prompt-check
                    (sleep-for 0.020); 0.1 is noticeable!
                  ;; else: default
                  ;; (message "sleep:%s" (and do-sleep (* 0.4 sleep)))
                  (ess-wait-for-process sprocess    ;; default timeout 30 seconds!
                                        (and do-sleep (* 0.4 sleep))) ;; not visible here! error during redisplay in *R* buffer
                  )
                ;; (message "command:%s" (process-get sprocess 'ready))
                (delete-region (point-at-bol) (point-max))
                )
              (ess-if-verbose-write " .. ok{ess-command2}\n")
              )
          ;; Restore old values for process filter
          (set-process-buffer sprocess oldpb)
          (set-process-filter sprocess oldpf)
          (set-marker (process-mark sprocess) oldpm oldpb) ;; need oldpb here!!! otherwise it is not set for some reason
          )
        )
      )
    ))

(defun ess-get-words-from-vector2 (command &optional no-prompt-check)
  "Evaluate the S command COMMAND, which returns a character vector.
Return the elements of the result of COMMAND as an alist of strings.
COMMAND need *NOT* have a terminating newline.

Improves on ess-get-words-from-vector by dealing with vectors
with arbitrary length. (max print problem) But assumes that none of
the words does contain ',!,' substring :)
"
  (let ((tbuffer (get-buffer-create
                  " *ess-get-words*")); initial space: disable-undo
        words end-pos start-pos)
    (save-excursion
      (set-buffer tbuffer)
      (ess-if-verbose-write (format "ess-get-words*(%s).. " command))
      (setq command (format "paste({%s}, collapse=',!,')\n" command))
      (ess-command2 command tbuffer 'sleep no-prompt-check)
      (ess-if-verbose-write " [ok] ..")
      (goto-char (point-min))
      (if (not (looking-at "[ +>]*\\[1\\]"))
          (progn (ess-if-verbose-write "not seeing \"[1]\".. ")
                 (setq words nil)
                 )
        (goto-char (point-min))
        (setq start-pos  (re-search-forward "\"" nil t))
        (goto-char (point-max))
        (setq end-pos (re-search-backward "\"" nil t))
        (setq words (split-string
                     (buffer-substring-no-properties start-pos end-pos)
                     ",!,"))
        )
      )
    ;;DBG, do *not* (i.e., comment):
    ;; (kill-buffer tbuffer)
    (ess-if-verbose-write
     (if (> (length words) 5)
         (format " |-> (length words)= %d\n" (length words))
       (format " |-> words= '%s'\n" words)))
    (if (and (eq (length words) 1)
             (equal (car words) ""))
        nil
      words)
    ))


;;;_ * Kludges and Fixes

;;; delete-char and delete-backward-car do not delete whole intangible text
(defadvice delete-char (around delete-backward-char-intangible activate)
  "When about to delete a char that's intangible, delete the whole intangible region
Only do this when #chars is 1"
  (if (and (= (ad-get-arg 0) 1)
           (get-text-property (point) 'intangible))
      (progn
        (kill-region (point) (next-single-property-change (point) 'intangible))
        (indent-for-tab-command)
        )
    ad-do-it
    ))

(defadvice delete-backward-char (around delete-backward-char-intangible activate)
  "When about to delete a char that's intangible, delete the whole intangible region
Only do this when called interactively and  #chars is 1"
  (if (and (= (ad-get-arg 0) 1)
           (> (point) (point-min))
           (get-text-property (1- (point)) 'intangible))
      (progn
        (kill-region (previous-single-property-change (point) 'intangible) (point))
        (indent-for-tab-command)
        )
    ad-do-it
    ))

;;; previous-line gets stuck if next char is intangible
(defadvice previous-line (around solves-intangible-text-kludge activate)
  "When about to move to previous line when next char is
intanbible, step char backward first"
  (if (and (or (null (ad-get-arg 0))
               (= (ad-get-arg 0) 1))
           (get-text-property (point) 'intangible))
      (backward-char 1)
    )
  ad-do-it
  )

;;TODO
;; (defun ess-bp-remove-all-all-buffers nil
;;   "Delete all visible breakpoints in all open buffers."
;;   (interactive)
;;   (let ((buffers (buffer-list)))
;;     (save-excursion
;;       (while buffers
;;         (set-buffer (car buffers))
;;         (ess-bp-remove-all-current-buffer)
;;         (setq buffers (cdr buffers))))))



(ess-if-verbose-write "\n<- debug done")
(provide 'ess-tracebug)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ess-tracebug.el ends here