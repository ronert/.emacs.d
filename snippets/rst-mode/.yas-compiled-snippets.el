;;; Compiled snippets and support files for `rst-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'rst-mode
                     '(("auto" ".. autoclass:: $0\n   ${1::members: ${2:members}}" "autoclass" nil nil nil nil nil nil)
                       ("auto" ".. autofunction:: $0" "autofunction" nil nil nil nil nil nil)
                       ("auto" ".. automodule:: ${1:module_name}" "automodule" nil nil nil nil nil nil)
                       ("cls" ":class:\\`$0\\`" "class" nil nil nil nil nil nil)
                       ("graph" ".. digraph:: $1\n\n   $0" "digraph" nil nil nil nil nil nil)
                       ("fun" ":function:\\`$0\\`" "function" nil nil nil nil nil nil)
                       ("graph" ".. graph:: $1\n\n   $0" "graph" nil nil nil nil nil nil)
                       ("graph" ".. graphviz::\n\n   $0" "graphviz" nil nil nil nil nil nil)
                       ("img" ".. image:: ${1:path}\n   :height: ${2:100}\n   :width: ${3:200}\n   :alt: ${4:description}\n\n$0" "image" nil nil nil nil nil nil)
                       ("inh" ".. inheritance-diagram:: $0" "inheritance" nil nil nil nil nil nil)
                       (":" ":${1:var}: $0" "meta" nil nil nil nil nil nil)
                       ("mod" ":mod: \\`$0\\`" "module" nil nil nil nil nil nil)
                       ("src" ".. parsed-literal::\n   $0" "parsed_literal" nil nil nil nil nil nil)
                       ("term" ":term:\\`$0\\`" "term" nil nil nil nil nil nil)
                       ("url" ".. _${1:description}: $0" "url" nil nil nil nil nil nil)
                       ("|" "| $0\n|" "verbatim" nil nil nil nil nil nil)
                       ("warn" ".. warning:\n   $0" "warning" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Thu Dec 26 11:33:14 2013
