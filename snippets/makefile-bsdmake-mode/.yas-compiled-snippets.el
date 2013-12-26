;;; Compiled snippets and support files for `makefile-bsdmake-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'makefile-bsdmake-mode
                     '(("phony" ".PHONY: $0" "PHONY" nil nil nil nil nil nil)
                       ("echo" "@echo ${1:\"message to output\"}\n" "echo" nil nil nil nil nil nil)
                       ("gen" "all: ${1:targets}\n\n$0\n\nclean:\n        ${2:clean actions}\n" "gen" nil nil nil nil nil nil)
                       ("if" "@if [ ${1:cond} ]\n    then $0\nfi\n" "if" nil nil nil nil nil nil)
                       ("$" "$(${1:VAR})$0" "var" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Thu Dec 26 11:33:14 2013
