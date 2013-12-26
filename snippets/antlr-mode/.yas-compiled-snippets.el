;;; Compiled snippets and support files for `antlr-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'antlr-mode
                     '(("proj" "<project name=\"${1:test}\" default=\"${2:compile}\" basedir=\"${3:.}\">\n\n$0\n</project>" "project" nil nil nil nil nil nil)
                       ("prop" "<property name=\"${1:name}\" value=\"${2:value}\" />\n$0" "property" nil nil nil nil nil nil)
                       ("target" "<target name=\"${1:compile}\" ${2:other}>\n        $0\n</target>" "target" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Thu Dec 26 11:33:13 2013
