;; This is an example of how you could set up this file. This setup
;; requires a directory called util in the project root and that the
;; util directory contains the testing tools ert and espuds.

(let* ((features-directory
        (file-name-directory
         (directory-file-name (file-name-directory load-file-name))))
       (project-directory
        (file-name-directory
         (directory-file-name features-directory))))
  (setq emacs-killlines-root-path project-directory)
  (setq emacs-killlines-util-path (expand-file-name "util" emacs-killlines-root-path)))

(add-to-list 'load-path emacs-killlines-root-path)
(add-to-list 'load-path (expand-file-name "espuds" emacs-killlines-util-path))
(add-to-list 'load-path (expand-file-name "ert" emacs-killlines-util-path))

(require 'kill-lines)
(require 'espuds)
(require 'ert)


(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
